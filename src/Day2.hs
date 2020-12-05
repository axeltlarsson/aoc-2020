{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( solve
  , passwordPolicy
  , parser
  ) where

import qualified Data.Text as T
import Text.ParserCombinators.ReadP

solve = do
  input <- readFile "data/day2_full.txt"
  let parsed = parse $ lines input
  let valids = filter validPassword2 parsed
  print $ length valids

parse :: [String] -> [Password]
parse ls = map (fst . head) $ map (readP_to_S parser) ls

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

passwordPolicy :: ReadP (Int, Int, Char)
passwordPolicy = do
  min <- number
  string "-"
  max <- number
  string " "
  ch <- satisfy (\char -> char >= 'a' && char <= 'z')
  return (min, max, ch)

{- example:
 - 1-3 a: abcde
 - 1-3 b: cdefg
 - 2-9 c: ccccccccc
 -}
data Password =
  Password
    { min :: Int
    , max :: Int
    , ch :: Char
    , pswd :: String
    }
  deriving (Show)

password :: ReadP String
password = many1 (satisfy (\char -> char >= 'a' && char <= 'z'))

parser :: ReadP Password
parser = do
  (min, max, ch) <- passwordPolicy
  string ": "
  pswd <- password
  eof
  return (Password min max ch pswd)

validPassword :: Password -> Bool
validPassword (Password min max ch pswd) =
  let occurrences = length $ filter (== ch) pswd
   in occurrences >= min && occurrences <= max

validPassword2 :: Password -> Bool
validPassword2 (Password min max ch pswd) =
  let fstCh = T.index (T.pack pswd) (min - 1)
      sndCh = T.index (T.pack pswd) (max - 1)
   in fstCh == ch && sndCh /= ch || sndCh == ch && fstCh /= ch
