{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Battleship where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Random
import Control.Monad.Trans.State
import Data.List as L
import Data.List.Split (splitOn, chunksOf)
import Data.Ord
import Data.Text as T
import Data.Text.Read as R
--import Debug.Trace
import Control.Lens
import Language.Haskell.TH
import System.Random

type GeneratorState = State StdGen

getRandom :: Random a => GeneratorState a
getRandom = do generator <- get
               let (value, newGenerator) = random generator
               put newGenerator
               return value

data Orientation = Horizontal
                  | Vertical deriving (Eq, Show, Enum, Bounded)

instance Random ShipType where
         random g  = case randomR (fromEnum (minBound :: ShipType), fromEnum (maxBound :: ShipType)) g of
                       (r, g') -> (toEnum r, g')
         randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                              (r, g') -> (toEnum r, g')

instance Random Orientation where
         random g  = case randomR (fromEnum (minBound :: Orientation), fromEnum (maxBound :: Orientation)) g of
                       (r, g') -> (toEnum r, g')
         randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                              (r, g') -> (toEnum r, g')

data ShipType = Carrier
               | Battleship
               | Submarine
               | Cruiser
               | Destroyer deriving (Eq, Show, Enum, Bounded)

data Ship = Ship { orientation :: Orientation
                  , category :: ShipType
                  , _damage :: [Int]
                  , x :: Int
                  , y :: Int } deriving (Eq, Show)

instance Random Ship where
         random = runRand $ Ship <$> liftRand random <*> liftRand random <*> pure [] <*> liftRand random <*> liftRand random
         -- I want the last two arguments to be Ints between a & b (or some specifiable range), but it seems like a & b are some other kind of type?
         --randomR (a, b) = runRand $ Ship <$> liftRand random <*> liftRand random <*> pure [] <*> liftRand (randomR a b) <*> liftRand (randomR a b)

data Spot = Maybe Ship deriving (Show)

data Board = Board { _spots :: [Ship]
                   , columns :: Int
                   , rows :: Int } deriving (Show)

makeLenses ''Board     
makeLenses ''Ship

getDamage :: Ship -> [Int]
getDamage = view damage

setDamage :: [Int] -> Ship -> Ship
setDamage damages ship = ship { _damage = damages }

updateDamage :: ([Int] -> [Int]) -> Ship -> Ship
updateDamage f ship = setDamage (f $ getDamage ship) ship

sizeOf :: Ship -> Int
sizeOf ship = case ty of
                Carrier -> 5
                Battleship -> 4
                Submarine -> 3
                Cruiser -> 3
                Destroyer -> 2
              where ty = category ship

letterMarker :: Ship -> Int -> Int -> String
letterMarker ship ox oy = if isDamaged
                          then "!!"
                          else case ty of
                               Carrier -> "CC"
                               Battleship -> "BB"
                               Submarine -> "SS"
                               Cruiser -> "RR"
                               Destroyer -> "DD"
                        where ty = category ship
                              sx = x ship
                              sy = y ship
                              offset = case orientation ship of
                                         Horizontal -> ox - sx
                                         Vertical -> oy - sy
                              isDamaged = offset `elem` getDamage ship

shipAt :: Board -> Int -> Int -> [Ship]
shipAt board x y = [s | s <- getShips board, not $ L.null ([(x, y)] `intersect` shipCoords s)]

shipTypeAt :: Board -> Int -> Int -> String
shipTypeAt board sx sy = case ship of
  [] -> "  " --show x ++ show y -- for debugging grid
  s -> letterMarker (L.head s) sx sy
  where ship = shipAt board sx sy

boardToStrings' :: Board -> [String]
boardToStrings' board = do
  y <- [0.. (columns board)]
  x <- [0.. (rows board)]
  return $ shipTypeAt board x y

boardToStrings :: Board -> [[String]]
boardToStrings board = Data.List.Split.chunksOf (columns board) $ boardToStrings' board

printBoard :: Board -> IO ()
printBoard board = mapM_ print (boardToStrings board)

getShips :: Board -> [Ship]
getShips = view spots

setShips :: [Ship] -> Board -> Board
setShips ships board = board { _spots = ships }

updateShips :: ([Ship] -> [Ship]) -> Board -> Board
updateShips f board = setShips (f $ getShips board) board

shipCoords :: Ship -> [(Int, Int)]
shipCoords ship = L.zip xs ys
           where mapper f = L.map (+ f ship) [0.. (sizeOf ship - 1)]
                 [xs, ys] = case orientation ship of
                                 Vertical -> [repeat $ x ship, mapper y]
                                 Horizontal -> [mapper x, repeat $ y ship]

isCollision :: Board -> Ship -> Bool
isCollision board ship = case newCoords `intersect` allCoords of
                           [] -> False
                           _ -> True
                      where newCoords = shipCoords ship
                            allCoords = L.concatMap shipCoords (_spots board)

canAddShip :: Board -> Ship -> Bool
canAddShip board ship = not (isCollision board ship)

addShip :: Board -> Ship -> Board
addShip board ship = if canAddShip board ship
                        then updateShips (++ [ship]) board
                        else board

isShipAlive :: Ship -> Bool
isShipAlive ship = sizeOf ship > L.length (getDamage ship)

isGameOver :: Board -> Bool
isGameOver board = 0 == L.length unsunkShips
    where unsunkShips = [s | s <- getShips board, isShipAlive s]

shipCount :: Board -> Int
shipCount board = L.length $ getShips board

fireAt :: Board -> Int -> Int -> Board
fireAt board fx fy = updateShips shipsHelper board
              where shipsHelper = \ships -> case ship of
                                              [] -> ships
                                              [s] -> (ships \\ [s]) ++ [updateDamage (nub . (\ds -> offset : ds)) s]
                                                  where offset = case orientation s of
                                                                   Horizontal -> fx - x s
                                                                   Vertical -> fy - y s
                                           where ship = shipAt board fx fy

playGame :: Board -> IO ()
playGame board = do
         printBoard board
         if isGameOver board
         then putStrLn "Good job, game over!"
         else do
              putStrLn "Battleship Coords (e.g. x,y):"
              t <- getLine
              let coords = T.splitOn (T.pack ",") (T.pack t)
              when (not $ L.null coords) $ do
                    let [x,y] = L.map (\c -> read (T.unpack c) :: Int) coords
                    playGame $ fireAt board (x - 1) (y - 1)



import Data.Ord 

ex13 :: Int -> Int -> Int -> Int
ex13 x y z = square a + square b
     where square i = i * i
           (a : b : _) = sortBy (comparing negate) [x, y, z]

ex13General :: Int -> [Int] -> Int
ex13General n xs = foldr1 (+) squaredTop
            where square i = i * i
                  top = take n (sortBy (comparing negate) xs)
                  squaredTop = map square top

-- *Battleship> ex13General 4 [6, 5, 1, 2, 3, 4]
-- 86
-- *Battleship> ex13General 2 [6, 5, 1, 2, 3, 4]
-- 61


-- ex13 1 2 3 -- => 13
-- ex13 2 3 1 -- => 13
-- ex13 3 2 1 -- => 13

-- ex13 4 3 4 -- => 32
-- ex13 4 4 3 -- => 32
-- ex13 3 4 4 -- => 32

-- ex13 2 2 3 -- => 13
-- ex13 2 3 2 -- => 13
-- ex13 3 2 2 -- => 13

-- ex13 4 6 5 -- => 61
