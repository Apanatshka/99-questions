{- |
Module      :  Main
Description :  Implementation of Problem 2 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Find the last but one element of a list.
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

simple solution given. 
-}

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs