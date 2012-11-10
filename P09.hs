{- |
Module      :  Main
Description :  Implementation of Problem 9 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Pack consecutive duplicates of list elements into sublists. If a list contains
 repeated elements they should be placed in separate sublists.
*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]

Derived both implementations from my implementations of compress from question
 8. 
This function is also built-in as group. 
-}

import Data.List(group)

pack :: Eq a => [a] -> [[a]]
pack []                = []
pack (x:y:xs) | x == y = (x:h):t
  where (h:t) = pack (y:xs)
pack (x:xs)            = [x] : pack xs

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' l@(x:_) = p1 : pack' rest
  where (p1, rest) = span (== x) l