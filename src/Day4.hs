module Day4
  ( solve
  , parser
  ) where

import qualified Data.Text as T
import Text.ParserCombinators.ReadP

{-
 - example input:
 -
 - ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
 - byr:1937 iyr:2017 cid:147 hgt:183cm
 -
 - iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
 - hcl:#cfa07d byr:1929
 -
 - hcl:#ae17e1 iyr:2013
 - eyr:2024
 - ecl:brn pid:760753108 byr:1931
 - hgt:179cm
 -
 - hcl:#cfa07d eyr:2025 pid:166559648
 - iyr:2011 ecl:brn hgt:59in
 -}
{-
 - Each passport is represented as a sequence of key:value pairs
 - separated by spaces or newlines. Passports are separated by blank lines.
 -}
{-
 - fields:
 -     byr (Birth Year)
 -     iyr (Issue Year)
 -     eyr (Expiration Year)
 -     hgt (Height)
 -     hcl (Hair Color)
 -     ecl (Eye Color)
 -     pid (Passport ID)
 -     cid (Country ID)
 -}
solve = do
  input <- readFile "data/day4_small.txt"
  let str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  let parsed = parse str
  print str
  print parsed

parse :: String -> [(Passport, String)]
parse str = (readP_to_S parser) str

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

number :: ReadP Int
number = fmap read (many1 digit)

character :: ReadP Char
character = satisfy (\char -> char >= 'a' && char <= 'z')

stringP :: ReadP String
stringP = many1 character

stringField :: String -> ReadP String
stringField fieldName = do
  string (fieldName ++ ":")
  val <- stringP
  return val

numberField :: String -> ReadP String
numberField fieldName = do
  string (fieldName ++ ":")
  val <- number
  return $ show val

type Passport = (String, String)

parser :: ReadP Passport
parser = do
  field1 <-
    choice
      [ stringField "ecl"
      , stringField "eyr"
      , numberField "pid"
      , stringField "hcl"
      ]
  string " "
  field2 <-
    choice
      [ stringField "ecl"
      , stringField "eyr"
      , numberField "pid"
      , stringField "hcl"
      ]
  return (field1, field2)
