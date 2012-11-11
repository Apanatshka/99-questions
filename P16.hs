{- |
Module      :  Main
Description :  Implementation of Problem 16 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Drop every N'th element from a list.
*Main> dropEvery "abcdefghik" 3
"abdeghk"

Simple solution given.
-}

dropEvery :: [a] -> Int -> [a]
dropEvery l n
  | length l < n = l
  | otherwise    = l1 ++ dropEvery l2 n
    where (l1, (_:l2)) = splitAt (n-1) l