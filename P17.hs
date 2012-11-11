{- |
Module      :  Main
Description :  Implementation of Problem 17 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.
*Main> split "abcdefghik" 3
("abc", "defghik")

Simple solution given.
-}

split :: [a] -> Int -> ([a], [a])
split l n
  | length l <= n = (l, [])
  | n        <= 0 = ([], l)
  | otherwise     = split' l n
    where
      split' []     _ = ( [],    []  )
      split' (x:xs) 1 = ( [x],   xs  )
      split' (x:xs) n = ( x:xs1, xs2 )
        where (xs1,xs2) = split' xs (n-1)