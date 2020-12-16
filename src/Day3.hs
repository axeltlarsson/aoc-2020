{-# LANGUAGE OverloadedStrings #-}

module Day3
  ( solve
  ) where

import Data.Array
import qualified Data.Text as T

{-
 - example input:
 - ..##.......
 - #...#...#..
 - .#....#..#.
 - ..#.#...#.#
 - .#...##..#.
 - ..#.##.....
 - .#.#.#....#
 - .#........#
 - #.##...#...
 - #...##....#
 - .#..#...#.#
 -}
solve = do
  input <- readFile "data/day3_full.txt"
  let f = field (lines input)
  let slope11 = ride (0, 0) (1, 1) 0 f
  let slope31 = ride (0, 0) (3, 1) 0 f
  let slope51 = ride (0, 0) (5, 1) 0 f
  let slope71 = ride (0, 0) (7, 1) 0 f
  let slope12 = ride (0, 0) (1, 2) 0 f
  print $ slope11 * slope31 * slope51 * slope71 * slope12

type Field = Array Int (Array Int Char)

field :: [String] -> Field
field input =
  let fromString str = listArray (0, length str - 1) str
   in fmap fromString $ listArray (0, length input - 1) input

tree :: (Int, Int) -> Field -> Bool
tree (x, y) f =
  let width = 1 + (snd (bounds (f ! 0)))
   in (f ! y) ! (rem x (width)) == '#'

ride :: (Int, Int) -> (Int, Int) -> Int -> Field -> Int
ride (x, y) (xDiff, yDiff) trees f =
  let (x', y') = (x + xDiff, y + yDiff)
      t =
        if tree (x', y') f
          then 1
          else 0
   in if y' > (snd $ bounds f)
        then trees
        else ride (x', y') (xDiff, yDiff) (trees + t) f
