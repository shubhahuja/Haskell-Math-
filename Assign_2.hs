{- Assignment 2
 - Name: `Shubham Ahuja`
 - Date: 21 october 2018
 -}
module Assign_2 where
import Data.List
macid :: String
macid = "ahujas6"

type Vector = (Double,Double,Double)

{-vecZero of type Vector that implements the zero vector constant. and returns a
vector of magnitude zero
 -}
vecZero :: Vector
vecZero = (0,0,0)

{-vecScalarProd of type Double ->Vector -> Vector that implements the
scalar product function.
where scalar product means multiplying a vector by a constant
 -}
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd x (a,b,c) =(a*x,b*x,c*x)


{- vecSum of type Vector -> Vector-> Vector that implements the sum function.
which takes 2 vectors and return a vector which has sum of x,y, and z coordinates
of both input vectors
 -}


vecSum :: Vector -> Vector -> Vector
vecSum (a1,b1,c1) (a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

{- vecMagnitude of type Vector ->Double that implements the magnitude function.
The magnitude of V is the real number sqrt(a2 + b2 + c2)
 -}
vecMagnitude :: Vector -> Double
vecMagnitude (a,b,c) = (a*a +b*b + c*c)**(1/2)

{- vecInnerProd of type Vector ->Vector -> Double that implements the
inner product function.
The inner product of V and V 0 (also called the dot product) is the real
number aa0 + bb0 + cc0.
 -}
vecInnerProd :: Vector -> Vector -> Double
vecInnerProd (a1,b1,c1) (a2,b2,c2) = a1*a2 +b1*b2 +c1*c2

{-vecDistance takes 2 vectors as input and returns a Double which is the distance
 between the 2 input vectors
where The distance between V and V0 is the magnitude of the
difference of V and V0.
-}
vecDistance:: Vector->Vector->Double
vecDistance (a1,b1,c1) (a2,b2,c2)= vecMagnitude(a1-a2,b1-b2,c1-c2)


{-vecDisList takes a vector and a list of vectors and returns a list with distance
between the input vector and each vector from the input list .
it uses recursion as it calls vecDisList again inside itself
-}
vecDisList::Vector->[Vector]->[Double]
vecDisList v0 []=[]
vecDisList v0 (x:xs)=[vecDistance v0 x] ++ vecDisList v0 xs

{-vecMinMax takes  a list and retuns a list with index of maximum value from list and
minimum value from the list
-}
vecMinMax::Ord a =>[a] -> [Maybe Int]
vecMinMax xs= [elemIndex (maximum xs) xs, elemIndex (minimum xs) xs]

{- vecTupList takes  a pair in form of List and changes it into a tuple

-}
vecTupList :: [Vector] -> (Vector, Vector)
vecTupList [a,b]=(a,b)


{- vecF takes a vector and a list as Input and retuns a tuple with 2 vectors.

vecF x y equals a pair (v1, v2) of values of type Vector such that
v1 is a member in the list y whose distance between x and itself
is less than or equal to the distance between x and every other
member of y.
v2 is a member in the list y whose distance between x and itself is
greater than or equal to the distance between x and every other
member of y.
 -}
vecF :: Vector -> [Vector] -> (Vector,Vector)
vecF v0 []= (vecZero,vecZero)
vecF v0 xs= (b,a)
  where
    ys=vecDisList v0 xs
    t=vecMinMax ys
    t2=map (\(Just i)->i) t --we use a lambda function to change type from Just Int to Int
    a=xs !! (head t2)       -- we need to change type because !! takes only Int
    b=xs !! (last t2)
  

{-

TEST FOR vecScalarProd

Function: vecScalarProd
Test Case Number: 1
Input: vecScalarProd (-2) (1,2,3)
Expected Output: (-2.0,-4.0,-6.0)
Actual Output: (-2.0,-4.0,-6.0)

Function: vecScalarProd
Test Case Number: 2
Input: vecScalarProd 0 (1,2,3)
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)


Function:vecScalarProd
Test Case Number: 3
Input: vecScalarProd (-0.5) (1,-1,2)
Expected Output: (-0.5,0.5,-1.0)
Actual Output: (-0.5,0.5,-1.0)


TEST FOR vecSum

Function: vecSum
Test Case Number: 1
Input: vecSum (1,2,3) (1,1,1)
Expected Output: (2.0,3.0,4.0)
Actual Output: (2.0,3.0,4.0)


Function: vecSum
Test Case Number: 2
Input: vecSum (-1,2,-3) (1,1,1)
Expected Output:(0.0,3.0,-2.0)
Actual Output: (0.0,3.0,-2.0)

Function: vecSum
Test Case Number: 3
Input: vecSum (-1,2,-3) (0,0,1)
Expected Output: (-1.0,2.0,-2.0)
Actual Output: (-1.0,2.0,-2.0)



TEST FOR vecMagnitude

Function: vecMagnitude
Test Case Number: 1
Input: vecMagnitude (0,0,0)
Expected Output: 0.0
Actual Output: 0.0

Function: vecMagnitude
Test Case Number: 2
Input: vecMagnitude (-1,-2,2)
Expected Output: 3.0
Actual Output: 3.0

Function: vecMagnitude
Test Case Number: 3
Input: vecMagnitude (-1,0,2)
Expected Output: 2.23606797749979
Actual Output: 2.23606797749979

TEST FOR vecInnerProd

Function: vecInnerProd
Test Case Number: 1
Input: vecInnerProd (1,2,3) (1,1,0)
Expected Output: 3.0
Actual Output: 3.0

Function: vecInnerProd
Test Case Number: 2
Input: vecInnerProd (1,-1,-1) (1,10,1)
Expected Output: -10.0
Actual Output: -10.0

Function: vecInnerProd
Test Case Number: 3
Input: vecInnerProd (1,-1,-1) (0,0,0)
Expected Output: 0.0
Actual Output: 0.0



TEST FOR vecF


Function: vecF
Test Case Number: 1
Input: vecF (1,9,8) []
Expected Output: ((0.0,0.0,0.0),(0.0,0.0,0.0))
Actual Output: ((0.0,0.0,0.0),(0.0,0.0,0.0))

Function: vecF
Test Case Number: 2
Input: vecF (1,9,8) [(10,10,10),(8,9,3),(0,0,0),(-1,-1,-1)]
Expected Output: ((8.0,9.0,3.0),(-1.0,-1.0,-1.0))
Actual Output: ((8.0,9.0,3.0),(-1.0,-1.0,-1.0))

Function: vecF
Test Case Number: 3
Input: vecF (1,0,-1) [(10,10,10),(0,0,0),(-2,-2,-2),(-1,-1,-1)]
Expected Output: ((0.0,0.0,0.0),(10.0,10.0,10.0))
Actual Output: ((0.0,0.0,0.0),(10.0,10.0,10.0))

-}
