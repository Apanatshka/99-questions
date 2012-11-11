{- |
Module      :  Main
Description :  Implementation of Problem 14 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Duplicate the elements of a list.
> dupli [1, 2, 3]
[1,1,2,2,3,3]

Simple solution given.
-}

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs