{- |
Module      :  Main
Description :  Implementation of Problem 13 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly.
 I.e. don't explicitly create the sublists containing the duplicates, as in
 problem 9, but only count them. As in problem P11, simplify the result list by
 replacing the singleton lists (1 X) by X.
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

Already did that in P11 (this is just a paste of my solution). 
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