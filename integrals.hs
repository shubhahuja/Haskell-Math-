

{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - it takes a lower limit , a upper limit, a function and number of trapezoids to be used
 - it gives back the definite integral though this is just an approximation which gets better on increasing
 - number of trapezoids
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer->Double
definiteIntegral a b g n =if n<=0 then undefined else if a>=b then 0 else ans
  where
    i=(b-a)/(fromInteger n)
    ans= (def2 a (b) g n i)*i   {-i compute the last interval seperately to solve floating point error-}


{-it takes a lower limit , upper limit , function, number of trapezoids and
-f which is difference in upper and lower limit divided by number of trapezoids
- it returns reiman sum which is nothing but integral didvided by f
-}
def2:: (Fractional t1, Ord t2, Floating t2) => t2 -> t2 -> (t2 -> t1) -> t -> t2 -> t1
def2 a b g n f=if abs((a)-b)<10**(-10) then 0 else ans  {-we use a tolerance here so as to make sure it doesnt compute one more time than it should-}
  where
    a2=(a+f)
    z=((g a)+ (g a2))/2
    ans= (z + (def2 a2 b g n f ))



{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - funH takes an integer n and computes area between 0 to 1 for the functions
 - x^n and x^(1/n)
 - area between 2 curves is difference of integral
 - and area is always positive so we use abs
 - for number of trapezoids we use a random large number to increase accuracy
 -}



funH :: Integer -> Double
funH n = abs ans
  where
    a1= definiteIntegral 0 1 (\x->x^n) 100690
    a2=definiteIntegral 0 1 (\x->x**(1/(fromInteger n))) 100690
    ans=(a1-a2)

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - it takes a double n and compute area between the curve n^x from -1 to 1 and x axis
 - which is just integral of the function
 -as number of slices we use a random large number to increase accuracy
 -}

funK :: Double -> Double
funK n = definiteIntegral (-1) 1 (\x->n**x) 100690



{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function:definiteIntegral
 - - Test Case Number: 1
 - - Input:definiteIntegral (-19) 0 (\x->x^2) 10
 - - Expected Output:2297.7650000000003
 - - Acutal Output:2297.7650000000003
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:definiteIntegral
 - - Test Case Number:2
 - - Input:definiteIntegral (0) 9 (\x->3**x) 16
 - - Expected Output:18481.867082477685
 - - Acutal Output:18481.867082477685
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:definiteIntegral
 - - Test Case Number:3
 - - Input:definiteIntegral (1) 8 (\x->log x) 17
 - - Expected Output:9.623245533569436
 - - Acutal Output:9.623245533569436
 - -----------------------------------------------------------------



 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:1
 - - Input:funH 3
 - - Expected Output:0.49999994014131044
 - - Acutal Output:0.49999994014131044
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:2
 - - Input:funH (8)
 - - Expected Output:0.7777768316151968
 - - Acutal Output:0.7777768316151968
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:3
 - - Input:funH (16)
 - - Expected Output:0.8823507685166936
 - - Acutal Output:0.8823507685166936
 - -----------------------------------------------------------------



 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:1
 - - Input:funK (1000)
 - - Expected Output:144.76468276712154
 - - Acutal Output:144.76468276712154
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:2
 - - Input:funK (10)
 - - Expected Output:4.299515371608016
 - - Acutal Output:4.299515371608016
 - -----------------------------------------------------------------

 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:3
 - - Input:funK (16.99)
 - - Expected Output:5.977191558054287
 - - Acutal Output:5.977191558054287
 - -----------------------------------------------------------------
prop3

 Function: funH
 Property: comparing with answer from right side rectangle formula which is used for approximation of integrals
          they both would give almost same answer at large number of intervals
 Actual Test Result: Pass

---------------------------------------------------------------------------------------------------------------------------
prop4

Function: funK
Property: comparing with answer from right side rectangle formula which is used for approximation of integrals
         they both would give almost same answer at large number of intervals
Actual Test Result: Pass

-----------------------------------------------------------------------------------------------------------------------------
prop2

Function: definiteIntegral
Property: integral from a to b is equal to integral from a to c plus integral from c to b for c between a and b
           (definite Integral Property)
Actual Test Result: Pass
 - -----------------------------------------------------------------
prop5

 Function: funK
 Property: definite integral property -integral of a^x= a^x/loga
 Actual Test Result: Pass

 -----------------------------------------------------------------------------------------------------------------------------
prop6

 Function: funH
 Property: definite Integral Property  -integral of x^n=(x^(n+1))/(n+1)
 Actual Test Result: Pass
  - -----------------------------------------------------------------

 propostion may fail for same values due to tolerance but can give the wrute result if
 number of trapezoids in function funH and funK is increased but that would increase the time too.
 -}
