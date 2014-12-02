module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import Battleship.Swallow.Test
import Battleship.Coconut.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ swallowSuite
            , coconutSuite
            ]
