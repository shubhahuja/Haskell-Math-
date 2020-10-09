{- Assignment 1
 - Name: TODO add full name
 - Date: TODO add of completion
 -}

module Assign_1_ExtraCredit where

import Data.Complex
-- import Data.Complex -- TODO uncomment me to use built-in Complex type
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid = "TODO: put your mac id here"

cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c =(3*a*c -b*b)/(9*a*a)

cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d= ((9*a*b*c)-(27*a*a*d)-(b*b*b))/(54*a*a*a)


cubicRoot :: Double-> Double
cubicRoot x= if x<0
                then (-((-x)**(1.0/3)))
                else (x)**(1.0/3)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicDisc here
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q*q*q +r*r

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicS here
 -}

cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r =cubicRoot ((r+(q*q*q+r*r)**0.5)):+0

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicT here
 -}
cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r = cubicRoot ((r-(q*q*q+r*r)**0.5)):+0

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: TODO add comments on cubicRealSolutions here
 -}
cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d =
  let q= cubicQ a b c
      r= cubicR a b c d
      s= cubicComplexS q r
      t= cubicComplexT q r
      d= cubicDisc q r
      x1= s + t- (b/(3*a):+0)
      x2=(s+t)/2 - (b/(3*a):+0)+((-3)**(1/2))*(s-t)/2
      x3=(s+t)/(2) + (b/(3*a):+0)+((-3)**(1/2))*(s-t)/2
  in [x1,x2,x3]
