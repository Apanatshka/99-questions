{- |
Module      :  Main
Description :  Implementation of Problem 4 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Find the number of elements of a list.
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13

simple solution given. 
Another one with folds. 
-}

import Data.List(foldl')

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl' (\acc _ -> acc + 1) 0

-- found a really nice one on the haskell wiki: myLength = sum . map (\_->1)