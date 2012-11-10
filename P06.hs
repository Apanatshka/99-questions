{- |
Module      :  Main
Description :  Implementation of Problem 6 from 99 Questions. 
Copyright   :  (c) Jeff Smits
License     :  MIT

Maintainer  :  jeff.smits@gmail.com
Stability   :  experimental
Portability :  portable

Find out whether a list is a palindrome. A palindrome can be read forward or
 backward; e.g. (x a m a x).
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True

The first implementation without library functions. Gradually reverses the list
 to see if half the reversed list equals the other half (also checking for a
 middle value).
The second implementation uses this same logic with library functions.
The third simple reverses the whole thing. 
The fourth implementation checks removes first and last, checks them and moves
 to the part left. Not the most handsome when using lists...
-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = isPalindrome' list []
  where 
    isPalindrome' [] [] = True
    isPalindrome' [] p2 = False
    isPalindrome' p@(x:xs) p2 = or [
      listEqual p p2,
      listEqual xs p2,
      isPalindrome' xs (x:p2)]
    
    listEqual [] [] = True
    listEqual (x:xs) (y:ys) = x == y && listEqual xs ys
    listEqual _ _ = False

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' list
  | even . length $ list = take half list == reverse (drop half list)
  | otherwise            = take half list == reverse (drop (half+1) list)
    where half = flip div 2 $ length list

isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' list = reverse list == list

isPalindrome''' :: Eq a => [a] -> Bool
isPalindrome''' [] = True
isPalindrome''' [x] = True
isPalindrome''' (x:xs) = x == last xs && isPalindrome'' (init xs)