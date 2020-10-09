{- Assignment 4 Extra Credit
 - Name:shubham ahuja
 - Date: 17th nov 2018
 -}
module Assign_4_ExtraCredit where

import Criterion.Main

macid = "ahujas6"


data Nat = Z
         | S Nat

data Digit = Zero
           | One deriving Show

data BinNat = Atom Digit
            | Compound BinNat Digit

natPrint :: Nat -> String
natPrint (Z)=show 0
natPrint ((S a))="S" ++ natPrint a



binNatPrint :: BinNat -> String
binNatPrint (Atom One)="1"
binNatPrint (Atom Zero)="0"
binNatPrint (Compound a b)= (binNatPrint a) ++ binNatPrint(Atom b)

binlist (Atom One)=[1]
binlist (Atom Zero)=[0]
binlist (Compound a b)=(binlist a) ++( binlist (Atom b))

binToInt :: [Int] -> Int
binToInt []=0
binToInt (x:xs)=(x)*(2^(length xs))+binToInt xs

toBin:: Int-> [Int]
toBin 0 = [0]
toBin n =  if (n `mod` 2) ==0 then (toBin (n `div` 2)) ++[0] else if n `mod` 2==1 then  (toBin (n `div` 2)) ++[1] else []


binPlus a b= (toBin c)
  where
    a1=binToInt (binlist a)
    b1=binToInt (binlist b)
    c=a1+b1
