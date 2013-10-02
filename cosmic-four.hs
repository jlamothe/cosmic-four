module Main where

import Data.Char
import Safe (readMay)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  getInt "Enter a number: " >>= display . cosmicChain

getInt :: String -> IO Int
getInt prompt = do
  putStr prompt
  line <- getLine
  case readMay line of
    Just x  -> return x
    Nothing -> do
      putStrLn "That's not a number."
      getInt prompt

display :: [Int] -> IO ()
display [x, y] = do
  putStrLn $ show x ++ " is " ++ show y
display (x : xs @ (y : _)) = do
  putStrLn $ show x ++ " is " ++ show y
  display xs

cosmicChain :: Int -> [Int]
cosmicChain 4 = [4, 4]
cosmicChain x = x : cosmicChain (cosmicNext x)

cosmicNext :: Int -> Int
cosmicNext x = countLetters $ spellInt x

countLetters :: String -> Int
countLetters = length . filter isAlpha

spellInt :: Int -> String
spellInt x
  | x < 0 = "negative " ++ spellInt (- x)
  | x == 0 = "zero"
  | otherwise =
    result $ until done next (x, 0, "")
  where
    result (_, _, str) = str
    done (x, _, _) = x == 0
    next (x, exp, str)
      | x `rem` 1000 == 0 =
        (x `div` 1000, succ exp, str)
      | str == "" =
        (x `div` 1000, succ exp, spellSegment (x `rem` 1000) exp)
      | otherwise =
        (x `div` 1000, succ exp, spellSegment (x `rem` 1000) exp ++ " " ++ str)

spellSegment :: Int -> Int -> String
spellSegment x exp
  | exp == 0 = spellGroup x
  | otherwise = spellGroup x ++ " " ++ nameExp exp

nameExp :: Int -> String
nameExp x = case x of
  1  -> "thousand"
  2  -> "million"
  3  -> "billion"
  4  -> "trillion"
  5  -> "quadrillion"
  6  -> "quintillion"
  7  -> "sextillion"
  8  -> "septillion"
  9  -> "octillion"
  10 -> "nonillion"
  11 -> "decillion"
  _  -> error "spellGroup doesn't know this number"

spellGroup :: Int -> String
spellGroup x
  | x < 20 = case x of
    1  -> "one"
    2  -> "two"
    3  -> "three"
    4  -> "four"
    5  -> "five"
    6  -> "six"
    7  -> "seven"
    8  -> "eight"
    9  -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
  | x < 100 =
    if x `rem` 10 == 0
    then case x of
      20 -> "twenty"
      30 -> "thirty"
      40 -> "fourty"
      50 -> "fifty"
      60 -> "sixty"
      70 -> "seventy"
      80 -> "eighty"
      90 -> "ninety"
    else spellGroup (x - x `rem` 10) ++ "-" ++ spellGroup (x `rem` 10)
  | x < 1000 =
      spellGroup (x `div` 100) ++ " hundred " ++ spellGroup (x `rem` 100)

-- jl
