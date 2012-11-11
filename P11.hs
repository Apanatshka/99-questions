{- |
Module      :  Main
Description :  Implementation of Problem 11 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Modified run-length encoding.
Modify the result of problem 10 in such a way that if an element has no
 duplicates it is simply copied into the result list. Only elements with
 duplicates are transferred as (N E) lists.
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

Derived both implementations from my implementations of pack from P10. 
-}

data RLEnc a = Multiple Int a | Single a deriving Show

encodeModified :: Eq a => [a] -> [RLEnc a]
encodeModified []                    = []
encodeModified (e:e2:rest) | e == e2 = (Multiple n e):t
  where
    (h:t) = encodeModified (e:rest)
    n' = case h of
      Single _     -> 1
      Multiple x _ -> x
    n = n'+1
encodeModified (e:rest)              = Single e : encodeModified rest

encodeModified' :: Eq a => [a] -> [RLEnc a]
encodeModified' [] = []
encodeModified' l@(e:_) = (Multiple n e) : encodeModified' rest
  where
    (p1, rest) = span (== e) l
    n = length p1