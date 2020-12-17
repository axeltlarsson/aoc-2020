{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( solve
  , parser
  , parseHgt
  , validHgt
  , validHcl
  , validatePassport2
  , validEcl
  , validPid
  , validEyr
  , validByr
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Text.ParserCombinators.ReadP
import Text.Read

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
  input <- readFile "data/day4_full.txt"
  let ps = T.splitOn "\n\n" $ T.pack input
  let parsed = map (parse . T.unpack) ps
  let valids = length $ filter (validPassport2 . fst) parsed
  print valids

validatePassport2 :: String -> Bool
validatePassport2 input = validPassport2 $ fst . head $ readP_to_S parser input

parse :: String -> (Passport, String)
parse str = head $ (readP_to_S parser) str

character :: ReadP Char
character = satisfy (\char -> char >= 'a' && char <= 'z')

stringP :: ReadP String
stringP = many1 character

alphaNum :: ReadP String
alphaNum = many1 $ satisfy (\c -> c >= 'a' && c <= 'z' || c >= '0' && c <= '9')

field :: ReadP (String, String)
field = do
  fieldName <- stringP
  string ":"
  val <- many1 $ satisfy (\c -> c /= ' ' && c /= ':' && c /= '\n')
  optional $ string " "
  optional $ string "\n"
  return (fieldName, val)

type Passport = [(String, String)]

parser :: ReadP Passport
parser = do
  x <- manyTill (field) (eof)
  return x

{-
- the only optional field is 'cid'
-}
validPassport :: Passport -> Bool
validPassport passport =
  length passport == 8 ||
  (length passport == 7 && all (\(x, y) -> x /= "cid") passport)

{-
 - You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:
 -
 -     byr (Birth Year) - four digits; at least 1920 and at most 2002.
 -     iyr (Issue Year) - four digits; at least 2010 and at most 2020.
 -     eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
 -     hgt (Height) - a number followed by either cm or in:
 -         If cm, the number must be at least 150 and at most 193.
 -         If in, the number must be at least 59 and at most 76.
 -     hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
 -     ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
 -     pid (Passport ID) - a nine-digit number, including leading zeroes.
 -     cid (Country ID) - ignored, missing or not.
 -
 - Your job is to count the passports where all required fields are both present and valid according to the above rules
 -}
getFieldValue :: Passport -> String -> Maybe String
getFieldValue passport fieldName =
  let f = filter (\(x, y) -> x == fieldName) passport
   in case f of
        (_, x):xs -> Just x
        [] -> Nothing

validPassport2 :: Passport -> Bool
validPassport2 passport =
  let val = getFieldValue passport
      byr = val "byr"
      byrNum = byr >>= readMaybe
      iyr = val "iyr"
      iyrNum = iyr >>= readMaybe
      eyr = val "eyr"
      eyrNum = eyr >>= readMaybe
      hgt = val "hgt"
      hcl = val "hcl"
      ecl = val "ecl"
      pid = val "pid"
   in (length passport == 7 || length passport == 8) &&
      (Maybe.fromMaybe False $ fmap validByr byrNum) &&
      (Maybe.fromMaybe False $ fmap validIyr iyrNum) &&
      (Maybe.fromMaybe False $ fmap validEyr eyrNum) &&
      (Maybe.fromMaybe False $ fmap validHgt hgt) &&
      (Maybe.fromMaybe False $ fmap validHcl hcl) &&
      (Maybe.fromMaybe False $ fmap validEcl ecl) &&
      (Maybe.fromMaybe False $ fmap validPid pid)

validByr :: Int -> Bool
validByr x = x >= 1920 && x <= 2002

validIyr :: Int -> Bool
validIyr x = x >= 2010 && x <= 2020

validEyr :: Int -> Bool
validEyr x = x >= 2020 && x <= 2030

pidParser :: ReadP String
pidParser = do
  x <- count 9 digit
  eof
  return x

validPid :: String -> Bool
validPid input = length (readP_to_S pidParser input) == 1

eclParser :: ReadP String
eclParser = do
  c <-
    Text.ParserCombinators.ReadP.choice
      [ string "amb"
      , string "blu"
      , string "brn"
      , string "gry"
      , string "grn"
      , string "hzl"
      , string "oth"
      ]
  return c

validEcl :: String -> Bool
validEcl input = length (readP_to_S eclParser input) == 1

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

hclParser :: ReadP String
hclParser = do
  string "#"
  x <-
    count 6 $
    satisfy (\char -> char >= '0' && char <= '9' || char >= 'a' && char <= 'f')
  return x

validHcl :: String -> Bool
validHcl input = length (readP_to_S hclParser input) == 1

hgtParser :: ReadP (Int, String)
hgtParser = do
  num <- many1 $ satisfy (\x -> x >= '0' && x <= '9')
  unit <- Text.ParserCombinators.ReadP.choice [string "cm", string "in"]
  return (read num, unit)

parseHgt :: String -> [((Int, String), String)]
parseHgt input = readP_to_S hgtParser input

validHgt :: String -> Bool
validHgt hgt =
  case parseHgt hgt of
    ((h, "cm"), _):xs -> h >= 150 && h <= 193
    ((h, "in"), _):xs -> h >= 59 && h <= 76
    (_:_) -> False
    [] -> False
