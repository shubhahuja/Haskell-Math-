
data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show




data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show

polyAltToPoly :: Num a => PolyAlt a -> Poly a
polyAltToPoly (Monomial b 0)= Coef b
polyAltToPoly (Monomial b 1)= Prod X (Coef b)
polyAltToPoly (Monomial b c)= Prod (polyAltToPoly(Monomial b 0)) (Prod X (polyAltToPoly (Monomial 1 ((abs c)-1))))
polyAltToPoly (SumAlt b c)= Sum ( polyAltToPoly (b)) (polyAltToPoly c)

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - polyValue takes a polynomial and a value and returns the value of
   that polynomial when x is equal to the input polyValue
   it uses recursion to do so.
   we know for coefficient it would be value of coefficient while for just x
   it would be the input value.
   in case of prod and sum it uses recursion to break down the big problem to
   small parts and hence return the value
 -}
polyValue ::Num a => Poly a -> a -> a
polyValue (X) a= a
polyValue (Prod b c) a=(polyValue b a)*(polyValue c a)
polyValue (Coef a) b= a
polyValue (Sum b c) a= (polyValue b a)+(polyValue c a)

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - polyDegree takes a polynomial and  returns degree of that polynomial
 degree of a polynomial is the highest power of variable x
 hence we know taht when input is a coefficient it would be 0 and when its just
 x it would be 1
 when product is done degree increases and hence we use recursion to find it
 when sum is done the polynomial is breaken down to find which part has highest
 degree and hence that is the degree of the the orignal polynomial
 -}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree (Coef a)=0
polyDegree (X)     =1
polyDegree (Prod b c)=(polyDegree b)+(polyDegree c)
polyDegree (Sum b c)=if (polyDegree b) >= (polyDegree c) then (polyDegree b) else (polyDegree c)


{- -----------------------------------------------------------------
 - polyDeriv
 - -----------------------------------------------------------------
 - polyDeriv takes a  polynomial and returns a polynomial that is derivative of
 the input polynomial
 this is doen with help of recursion too
 we know that derivative of a  coefficient is 0 and that of x is 1
 and according to derivative rules derivative of
 p(x)+g(x)= derivative of p(x) + derivative of g(x)
 and by product rule we know that
 derivative of f(x)*g(x)=(f(x)*g(x))'= f'(x)*g(x)+ f(x)*g'(x)
 and these rules are hence applied in defination of polyDeriv
 -}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Coef a)=Coef 0
polyDeriv (X)=Coef 1
polyDeriv (Sum b c)= Sum (polyDeriv b) (polyDeriv c)
polyDeriv (Prod b c)=Sum (Prod (polyDeriv b) c) (Prod b (polyDeriv c))

{-
polyAltValue takes a polynomial and a values and gives value of the that polynomial at
the input values
in this method polynomial is 1st changed to poly and then polyValue is called
-}
polyAltValue :: Num a => PolyAlt a -> a -> a
polyAltValue p n= polyValue (polyAltToPoly p) n


{-
polyAltDegree takes a polynomial and gives degree of the polynomial
in this method polynomial is 1st changed to poly and then polyDegree is called
-}

polyAltDegree :: (Num a, Eq a) => PolyAlt a -> Integer
polyAltDegree p =polyDegree (polyAltToPoly p)


{-
polyAltDeriv takes a polynomial and returns derivative of it
we know that derivative of x^n= n*x^(n-1)
this formula is applied in nthe given Function
-}

polyAltDeriv :: (Num a) => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial a 0 )=Monomial 0 0
polyAltDeriv (Monomial a b)= Monomial (a*(abs (fromInteger b))) (abs(b-1))
polyAltDeriv (SumAlt a b)= SumAlt (polyAltDeriv a) (polyAltDeriv b)

polyToPolyAlt:: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt (Coef a)= Monomial a 0
polyToPolyAlt (X)= Monomial  1 1
polyToPolyAlt (Prod (Coef a) (Coef b))=Monomial (a*b) 0
polyToPolyAlt (Sum a b)= SumAlt (polyToPolyAlt a) (polyToPolyAlt b)
polyToPolyAlt (Prod (a) b)=polyAltProd (polyToPolyAlt a) (polyToPolyAlt b)


{-
- polyAltProd takes 2 polynomials  and returns their product
 it does that by multiplying coefficient by coefficient and and variable variable

-}
polyAltProd :: (Num a, Eq a) => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial a b) (Monomial c d)=Monomial (a*c) (abs b+abs d)
polyAltProd (SumAlt a b) c= SumAlt (polyAltProd a c) (polyAltProd b c)
polyAltProd  c (SumAlt a b)= SumAlt (polyAltProd a c) (polyAltProd b c)
polyAltProd  (SumAlt c d) (SumAlt a b)= SumAlt (polyAltProd c (SumAlt a b)) (polyAltProd d (SumAlt a b))


{-
polyAltNewton p s t thus computes an approximate solution to the
polynomial equation p = 0
 we know that newton method gives the following formula
 Xn+1=Xn-f(X)/f'(X)
we apply the same formula here
-}

polyAltNewton::(Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t
  |(polyAltValue p s)<t = s
  | otherwise = polyAltNewton p w t where w= s-((polyAltValue p s))/(polyAltValue (polyAltDeriv p) s)




  {- -----------------------------------------------------------------
   - Test Cases
   - -----------------------------------------------------------------
   - TEST FOR polyAltValue
   - -----------------------------------------------------------------
   - - Function: polyAltValue
   - - Test Case Number:1
   - - Input: polyAltValue (Monomial 1 2) 3
   - - Expected Output:9
   - - Acutal Output:9

   - - Function: polyAltValue
   - - Test Case Number:2
   - - Input: polyAltValue (SumAlt (Monomial 1 2) (Monomial 1 1)) 3
   - - Expected Output:12
   - - Acutal Output:12

   - - Function:polyAltValue
   - - Test Case Number:3
   - - Input:polyAltValue (SumAlt (Monomial 1 2) (Monomial 1 0)) 3
   - - Expected Output:10
   - - Acutal Output:10
   - -----------------------------------------------------------------
   -TEST FOR polyAltDegree
   - -----------------------------------------------------------------
   - - Function:polyAltDegree
   - - Test Case Number:1
   - - Input:polyAltDegree (SumAlt (Monomial 1 2) (Monomial 1 0))
   - - Expected Output:2
   - - Acutal Output:2

   - - Function:polyAltDegree
   - - Test Case Number:2
   - - Input:polyAltDegree (SumAlt (Monomial 1 0) (Monomial 1 0))
   - - Expected Output:0
   - - Acutal Output:0

   - - Function:polyAltDegree
   - - Test Case Number:3
   - - Input:polyAltDegree (Monomial 1 7)
   - - Expected Output:7
   - - Acutal Output:7
   - -----------------------------------------------------------------
   - TEST FOR polyAltDeriv
   - -----------------------------------------------------------------
   - - Function:polyAltDeriv
   - - Test Case Number:1
   - - Input:polyAltDeriv (SumAlt (Monomial 1 0) (Monomial 1 0))
   - - Expected Output:SumAlt (Monomial 0 0) (Monomial 0 0)
   - - Acutal Output:SumAlt (Monomial 0 0) (Monomial 0 0)

   - - Function:polyAltDeriv
   - - Test Case Number:2
   - - Input:polyAltDeriv (Monomial 1 2)
   - - Expected Output:Monomial 2 1
   - - Acutal Output:Monomial 2 1

   - - Function:polyAltDeriv
   - - Test Case Number:3
   - - Input:polyAltDeriv (SumAlt (Monomial 1 1) (Monomial 1 20))
   - - Expected Output:SumAlt (Monomial 1 0) (Monomial 20 19)
   - - Acutal Output:SumAlt (Monomial 1 0) (Monomial 20 19)
   - -----------------------------------------------------------------
   -
   -TEST FOR polyAltProd
   - -----------------------------------------------------------------
   - - Function:polyAltProd
   - - Test Case Number:1
   - - Input:polyAltProd (Monomial 1 2) (Monomial 21 1)
   - - Expected Output:Monomial 21 3
   - - Acutal Output:Monomial 21 3

   - - Function:polyAltProd
   - - Test Case Number:2
   - - Input:polyAltProd (Monomial 1 0) (Monomial 0 1)
   - - Expected Output: Monomial 0 1
   - - Acutal Output:Monomial 0 1

   - - Function:polyAltProd
   - - Test Case Number:3
   - - Input:polyAltProd (SumAlt (Monomial 1 2) (Monomial 1 1)) (Monomial 1 1)
   - - Expected Output:SumAlt (Monomial 1 3) (Monomial 1 2)
   - - Acutal Output:SumAlt (Monomial 1 3) (Monomial 1 2)
   - -----------------------------------------------------------------
   - TEST FOR polyAltNewton
   - -----------------------------------------------------------------
   - - Function:polyAltNewton
   - - Test Case Number:1
   - - Input:polyAltNewton (Monomial 1 2) (1) 0.000001
   - - Expected Output:9.765625e-4
   - - Acutal Output:9.765625e-4

   - - Function:polyAltNewton
   - - Test Case Number:2
   - - Input:polyAltNewton (Monomial 2 3) 3 0
   - - Expected Output:9.620741997689759e-109
   - - Acutal Output:9.620741997689759e-109

   - - Function:polyAltNewton
   - - Test Case Number:3
   - - Input:polyAltNewton (Monomial 2 5) (1) 0
   - - Expected Output:1.8844256995627843e-65
   - - Acutal Output:1.8844256995627843e-65
   - -----------------------------------------------------------------
   -
   -TEST FOR polyToPolyAlt
   - -----------------------------------------------------------------
   - - Function:polyToPolyAlt
   - - Test Case Number:1
   - - Input:polyToPolyAlt (Prod X (Prod X X))
   - - Expected Output:Monomial 1 3
   - - Acutal Output: Monomial 1 3

   - - Function:polyToPolyAlt
   - - Test Case Number:2
   - - Input:polyToPolyAlt (Prod X (Sum X X))
   - - Expected Output:SumAlt (Monomial 1 2) (Monomial 1 2)
   - - Acutal Output:SumAlt (Monomial 1 2) (Monomial 1 2)

   - - Function:polyToPolyAlt
   - - Test Case Number:3
   - - Input:polyToPolyAlt (Sum X (Sum X X))
   - - Expected Output:SumAlt (Monomial 1 1) (SumAlt (Monomial 1 1) (Monomial 1 1))
   - - Acutal Output:SumAlt (Monomial 1 1) (SumAlt (Monomial 1 1) (Monomial 1 1))
   - -----------------------------------------------------------------
   - TEST FOR polyAltToPoly
   - -----------------------------------------------------------------
   - - Function:polyAltToPoly
   - - Test Case Number:1
   - - Input:polyAltToPoly (Monomial 1 2)
   - - Expected Output:Prod (Coef 1) (Prod X (Prod X (Coef 1)))
   - - Acutal Output:Prod (Coef 1) (Prod X (Prod X (Coef 1)))

   - - Function:polyAltToPoly
   - - Test Case Number:2
   - - Input:polyAltToPoly (SumAlt (Monomial 1 2) (Monomial 1 1))
   - - Expected Output:Sum (Prod (Coef 1) (Prod X (Prod X (Coef 1)))) (Prod X (Coef 1))
   - - Acutal Output:Sum (Prod (Coef 1) (Prod X (Prod X (Coef 1)))) (Prod X (Coef 1))

   - - Function:polyAltToPoly
   - - Test Case Number:3
   - - Input:polyAltToPoly (Monomial 1 0)
   - - Expected Output:Coef 1
   - - Acutal Output:Coef 1
   - -----------------------------------------------------------------
   -
   -}
