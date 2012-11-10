{- |
Module      :  Main
Description :  Implementation of Problem 3 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Find the K'th element of a list. The first element in the list is number 1.
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'

simple solution given. 
-}

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs (k-1)