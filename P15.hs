{- |
Module      :  Main
Description :  Implementation of Problem 15 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Replicate the elements of a list a given number of times.
> repli "abc" 3
"aaabbbccc"

Simple solution given.
Could have used replicate in stead of repli', but I think this is more
 efficient. 
-}

repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = repli' x n (repli xs n)
  where
    repli' x 0 l = l
    repli' x n l = repli' x (n-1) (x:l)