module Main where

import Battleship

s1 :: Ship
s1 = Ship Horizontal Carrier    [] 0  0 
s2 :: Ship
s2 = Ship Vertical   Carrier    [] 10 3
s3 :: Ship
s3 = Ship Horizontal Destroyer  [] 7  2
s4 :: Ship
s4 = Ship Horizontal Submarine  [] 4  3
s5 :: Ship
s5 = Ship Vertical   Battleship [5] 0 4

someShipWithALongName :: Ship
someShipWithALongName = Ship Vertical   Battleship [5] 0  4

startingBoard :: Board
startingBoard = Board { _spots = [--s1, s2, s3, s4, s5]
                           s1]
                       , columns = 13
                       , rows = 10}

main :: IO ()
main = playGame startingBoard
