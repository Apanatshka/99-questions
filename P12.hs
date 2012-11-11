{- |
Module      :  Main
Description :  Implementation of Problem 12 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Decode a run-length encoded list.
Given a run-length code list generated as specified in problem 11. Construct
 its uncompressed version.
P12> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"

Derived both implementations from my implementations of pack from P11. 
-}

data RLEnc a = Multiple Int a | Single a deriving(Show, Read)

decodeModified :: Eq a => [RLEnc a] -> [a]
decodeModified []                 = []
decodeModified ((Single e):t)     = e : decodeModified t
decodeModified ((Multiple 2 e):t) = e : decodeModified ((Single e):t)
decodeModified ((Multiple n e):t) = e : decodeModified ((Multiple (n-1) e):t)

decodeModified' :: Eq a => [RLEnc a] -> [a]
decodeModified' []                 = []
decodeModified' ((Single e):t)     = e : decodeModified' t
decodeModified' ((Multiple n e):t) = (replicate n e) ++ decodeModified' t