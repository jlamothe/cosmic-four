import Data.Char
import Safe (readMay)

main :: IO ()
main =
  getUnsigned "Enter a number: " >>= display . cosmicChain

getUnsigned :: String -> IO Int
getUnsigned prompt = do
  putStr prompt
  line <- getLine
  case readMay line of
    Just x  -> if x < 0 then tryAgain prompt else return x
    Nothing -> tryAgain prompt
  where
    tryAgain prompt = do
      putStrLn "That's not a number."
      getUnsigned prompt

display :: [Int] -> IO ()
display (x : xs @ (y : _)) = do
  putStrLn $ show x ++ " is " ++ show y
  display xs

cosmicChain :: Int -> [Int]
cosmicChain 4 = [4, 4]
cosmicChain x = x : cosmicChain (cosmicNext x)

cosmicNext :: Int -> Int
cosmicNext x = countLetters $ spellUnsigned x

countLetters :: String -> Int
countLetters = length . filter isAlpha

spellUnsigned :: Int -> String
spellUnsigned x
  | x < 0 = error "negative number passed to spellUnsigned"
  | x == 0 = "zero"
  | otherwise =
    result $ until done next (x, 0 , "")
  where
    result (_, _, str) = str
    done (x, _, _) = x == 0
    next (x, exp, str)
      | x `rem` 1000 == 0 =
        (x `div` 1000, succ exp, str)
      | str == "" =
        (x `div` 1000, succ exp, nameSegment (x `rem` 1000) exp)
      | otherwise =
        (x `div` 1000, succ exp, nameSegment (x `rem` 1000) exp ++ " " ++ str)

nameSegment :: Int -> Int -> String
nameSegment x exp
  | exp == 0 = nameGroup x
  | otherwise = nameGroup x ++ " " ++ nameExp exp

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
  _  -> error "nameGroup doesn't know this number"

nameGroup :: Int -> String
nameGroup x
  | x <= 13 = case x of
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
  | x < 20 = nameGroup (x - 10) ++ "teen"
  | x == 20 = "twenty"
  | x < 30 = "twenty-" ++ nameGroup (x - 20)
  | x == 30 = "thirty"
  | x < 40 = "thirty-" ++ nameGroup (x - 30)
  | x == 40 = "fourty"
  | x < 50 = "fourty-" ++ nameGroup (x - 40)
  | x == 50 = "fifty"
  | x < 60 = "fifty-" ++ nameGroup (x - 50)
  | x < 100 =
    if x `rem` 10 == 0
    then nameGroup (x `div` 10) ++ "ty"
    else nameGroup (x `div` 10) ++ "ty-" ++ nameGroup (x `rem` 10)
  | x < 1000 =
      nameGroup (x `div` 100) ++ " hundred " ++ nameGroup (x `rem` 100)

-- jl
