{- |
Module      :  Main
Description :  Implementation of Problem 19 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Rotate a list N places to the left.
Hint: Use the predefined functions length and (++).
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"

Using the modulus function to rotate N to N' where 0 <= N' < length list. 
-}

rotate :: [a] -> Int -> [a]
rotate []   _ = []
rotate list n = l2 ++ l1
  where
    n'       = n `mod` length list
    (l1, l2) = splitAt n' list