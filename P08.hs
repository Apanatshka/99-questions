{- |
Module      :  Main
Description :  Implementation of Problem 8 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy
 of the element. The order of the elements should not be changed.
> compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
["a","b","c","a","d","e"]

First implementation is simple.
Second uses library function dropWhile and seems to be faster. 
-}

compress :: Eq a => [a] -> [a]
compress []                = []
compress (x:y:xs) | x == y = compress (y:xs)
compress (x:xs)            = x : compress xs

compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress' . dropWhile (== x)) xs