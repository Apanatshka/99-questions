{- |
Module      :  Main
Description :  Implementation of Problem 10 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Run-length encoding of a list. Use the result of problem P09 to implement the
 so-called run-length encoding data compression method. Consecutive duplicates
 of elements are encoded as lists (N E) where N is the number of duplicates of
 the element E.
> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

Derived both implementations from my implementations of pack from P09. 
You could also map over the output of pack, but seemed boring. 
-}

encode :: Eq a => [a] -> [(Int, a)]
encode []                    = []
encode (e:e2:rest) | e == e2 = (n,e):t
  where
    ((n',_):t) = encode (e:rest)
    n = n'+1
encode (e:rest)            = (1,e) : encode rest

encode' :: Eq a => [a] -> [(Int, a)]
encode' [] = []
encode' l@(e:_) = (n,e) : encode' rest
  where
    (p1, rest) = span (== e) l
    n = length p1