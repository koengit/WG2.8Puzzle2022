module Sentence where

import Data.Char( toLower )

sentence :: [Int] -> String
sentence ks =
  --"This sentence contains "
  --"No sentence can possibly have exactly "
  --"This Haskell-generated sentence has "
  "This sentence generated in New York contains "
  ++ andIt [ number k ++ " "
                      ++ [c]
                      ++ if k > 1 then "'s" else ""
           | (k,c) <- ks `zip` ['a'..'z']
           , k > 0
           ]
  ++ "."
 where
  andIt [] = "- and -"
  andIt xs = concat [ x ++ ", " | x <- init xs ] ++ "and " ++ last xs

number :: Int -> String
number  1 = "one"
number  2 = "two"
number  3 = "three"
number  4 = "four"
number  5 = "five"
number  6 = "six"
number  7 = "seven"
number  8 = "eight"
number  9 = "nine"
number 10 = "ten"
number 11 = "eleven"
number 12 = "twelve"
number 13 = "thirteen"
number 15 = "fifteen"
number 18 = "eighteen"
number  k | k > 12 && k < 20 = number (k-10) ++ "teen"
number  k = decs !! (k `div` 10 - 2)
         ++ if k `mod` 10 == 0 then "" else "-" ++ number (k `mod` 10)
 where
  decs = ["twenty", "thirty", "forty", "fifty"] ++ error ("number " ++ show k)

count :: String -> [Int]
count s = [ length (filter ((c==) . toLower) s) | c <- ['a'..'z'] ]

check :: [Int] -> Bool
check xs = count (sentence xs) == xs

