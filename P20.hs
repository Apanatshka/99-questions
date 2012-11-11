{- |
Module      :  Main
Description :  Implementation of Problem 20 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  unstable
Portability :  portable

Remove the K'th element from a list.
*Main> removeAt 2 "abcd"
('b',"acd")

Simple (unsafe) solution, no cases for k < 1 and k > length list. 
-}

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x,xs)
removeAt k (x:xs) = (e,x:l)
  where (e,l) = removeAt (k-1) xs