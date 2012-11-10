{- |
Module      :  Main
Description :  Implementation of Problem 1 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Find the last element of a list.
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'

Simple solution given. 
-}

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs