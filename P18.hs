{- |
Module      :  Main
Description :  Implementation of Problem 18 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  unstable
Portability :  portable

Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements
 between the i'th and k'th element of the original list (both limits included).
 Start counting the elements with 1.
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"

Simple (unsafe) solution given. I don't feel like making up behaviour for
 i < 1. 
Another using library functions. 
-}

slice :: [a] -> Int -> Int -> [a]
slice [] _ _   = []
slice list 1 k
  | length list <= k = list
  | otherwise        = slice (init list) 1 k
slice list i k = slice (tail list) (i-1) (k-1)

slice' :: [a] -> Int -> Int -> [a]
slice' list i k = (take (k-i+1)) . drop (i-1) $ list