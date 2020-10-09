
import Data.List


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

data PolyAlt a = Monomial a Integer
                | SumAlt (PolyAlt a) (PolyAlt a)
                deriving Show

newtype PolyList a = PolyList [a] deriving Show

{-changes polylist to list-}
polyListtoList:: PolyList a-> [a]
polyListtoList (PolyList [x])= [x]
polyListtoList (PolyList (x:xs))= [x]++ (polyListtoList (PolyList xs))


{-changes string to Int
-}
stringLToIntL :: [String] -> [Integer]
stringLToIntL [a]=[read a:: Integer]
stringLToIntL (x:xs)=(stringLToIntL [x]) ++ (stringLToIntL xs)


{- it checks whether the list is a polynomial or not-}
listcheck :: (Num a, Num a1) => [a1] -> [a] -> ([a1], [a])
listcheck p1 q1=if (length p1)-(length q1)==0 then (p1,q1)
  else if (length p1)>(length q1) then (p1,(q1++(replicate ((length p1) -(length q1)) 0))) else ((p1++(replicate ((length q1) -(length p1)) 0)),q1)

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - it reads a file from given file path and makes a polylist with all the elements in that list
 -
 -}


getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do
  content <- readFile file
  return (PolyList (stringLToIntL (lines (content))))

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - it takes a polylist and a number and returns value of the polynomial at the given number value
 -}


polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue (PolyList [a]) n = a
polyListValue (PolyList (xs)) n= (head (reverse xs))*(n^(length (xs) -1)) + polyListValue (PolyList (init xs)) n


{-
- polyAltProd takes 2 polynomials  and returns their product
-it does that by multiplying coefficient by coefficient and and variable variable
-
-}

polyAltProd :: (Num a) => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial a b) (Monomial c d)=Monomial (a*c) (abs b+abs d)
polyAltProd (SumAlt a b) c= SumAlt (polyAltProd a c) (polyAltProd b c)
polyAltProd  c (SumAlt a b)= SumAlt (polyAltProd a c) (polyAltProd b c)
polyAltProd  (SumAlt c d) (SumAlt a b)= SumAlt (polyAltProd c (SumAlt a b)) (polyAltProd d (SumAlt a b))

{-polyListcheck checks whether a list is polynomial or not-}
polyListcheck :: (Eq a, Num a) => PolyList a -> PolyList a
polyListcheck (PolyList [])=(undefined)
polyListcheck (PolyList p1)= if p1== (replicate (length (p1)) 0) then  (undefined) else (PolyList p1)

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 -it takes a polylist and give degree of the polynomial
  -}

polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
polyListDegree (PolyList [])=undefined
polyListDegree (PolyList p1) =   (toInteger (length (p1)-1))


{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - it takes a polynomial in form of polylist  and returns its derivative in polylist form
 -}
polyListDeriv :: (Num a, Eq a) => PolyList a -> PolyList a
polyListDeriv (PolyList xs)= polyListcheck (PolyList (reverse (polyListDerivsub (PolyList xs))))

{- -----------------------------------------------------------------
 - polyListDerivsub
 - -----------------------------------------------------------------
 - it takes a polynomial in form of polylist  and returns its derivative in from of a list with elements
 -arranged in reverse order as in polylist
 -}

polyListDerivsub ::Num a=> PolyList a -> [a]
polyListDerivsub (PolyList [a])  =  []
polyListDerivsub (PolyList (xs)) =  ys
  where
    ys=([(head (reverse xs))*(fromInteger (toInteger (length xs -1)))] ++ (polyListDerivsub (PolyList (init xs))))

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - it takes 2 polylist and returns a polylist which is sum of both lists
 - the sum is such that each elemnts of 1st polylist is added with element from 2nd polylist with same index
 -}
polyListSum ::(Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList p1) (PolyList q1) = polyListcheck(PolyList (zipWith (+) (fst (y)) (snd (y))))
  where
    y=listcheck p1 q1

{-polylsitsum1 gives back a list even when list is full f 0-}
polyListSum1 (PolyList p1) (PolyList q1) =(PolyList (zipWith (+) (fst (y)) (snd (y))))
  where
    y=listcheck p1 q1

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 -  it takes 2 polylist and returns a polylist which is product of both lists
 - the product is such that  of 1st polylist is added with element from 2nd polylist
 -}

polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) (PolyList []) = undefined
polyListProd (PolyList []) ys = undefined
polyListProd xs (PolyList []) = undefined
polyListProd (PolyList [x]) (PolyList (ys)) = PolyList (map (x*) ys)
polyListProd (PolyList xs) (PolyList [y]) = PolyList (map (y*) xs)
polyListProd (PolyList (x:xs)) (PolyList ys) = polyListSum1 p1 p2
  where
    p1 = PolyList (map (*x) ys)
    p2 = polyListProd (PolyList (xs)) (PolyList (0:ys))






 {- polyAltToPoly
 - -----------------------------------------------------------------
 - it takes a polyAlt and changes it to a poly
 -}

polyAltToPoly :: Num a => PolyAlt a -> Poly a
polyAltToPoly (Monomial b 0)= Coef b
polyAltToPoly (Monomial b 1)= Prod X (Coef b)
polyAltToPoly (Monomial b c)= Prod (polyAltToPoly(Monomial b 0)) (Prod X (polyAltToPoly (Monomial 1 ((abs c)-1))))
polyAltToPoly (SumAlt b c)= Sum ( polyAltToPoly (b)) (polyAltToPoly c)

{- -----------------------------------------------------------------
 - polyToPolyAlt
 - -----------------------------------------------------------------
 - it takes a poly and chneges it to polyAlt
 -}
polyToPolyAlt:: (Num a) => Poly a -> PolyAlt a
polyToPolyAlt (Coef a)= Monomial a 0
polyToPolyAlt (X)= Monomial  1 1
polyToPolyAlt (Prod (Coef a) (Coef b))=Monomial (a*b) 0
polyToPolyAlt (Sum a b)= SumAlt (polyToPolyAlt a) (polyToPolyAlt b)
polyToPolyAlt (Prod (a) b)=polyAltProd (polyToPolyAlt a) (polyToPolyAlt b)

{- -----------------------------------------------------------------
 - polyListToPolyAlt
 - -----------------------------------------------------------------
 - it takes a polylist and changes it to polyAlt
 -}

polyListToPolyAlt :: Num a=> PolyList a -> PolyAlt a
polyListToPolyAlt (PolyList [])=(Monomial 0 0)
polyListToPolyAlt (PolyList xs)= SumAlt (Monomial (last xs) (toInteger ((length xs) -1))) (polyListToPolyAlt (PolyList (init xs)))

{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - it takes a polylist and changes it to poly
 -}

polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly p1 = polyAltToPoly (polyListToPolyAlt p1)

{- -----------------------------------------------------------------
 - polyAltListMaker
 - -----------------------------------------------------------------
 - it takes a polyAlt and makes a list of tuples in which ist element of tuple is coefficient and 2nd is power
 -}

polyAltListMaker :: PolyAlt t -> [(t, Int)]
polyAltListMaker (Monomial a b)=[(a,(fromInteger b))]
polyAltListMaker (SumAlt a b)= (polyAltListMaker a) ++ (polyAltListMaker b)

{- -----------------------------------------------------------------
 - simplist
 - -----------------------------------------------------------------
 - it takes a sorted list of tuples and then simplifies it to give back a list of tuples which has only one tuple with one type of power
 -}
simplist :: (Num a) => [(a, Int)] -> [(a, Int)]
simplist []=[]
simplist [(a,b)]=[(a,b)]
simplist (x:xs)= if (snd x)==(snd (head xs)) then (simplist (t:(tail xs))) else ( [x] ++ simplist (xs))
  where
    t=((fst x +(fst( head xs))) ,snd x)



{- -----------------------------------------------------------------
- polyListSort
- -----------------------------------------------------------------
- it takes a list of tuples and aort it according to 2nd elemnet of tuple
 -}
polyListSort :: Num t => [(t, Int)] -> [(t, Int)]
polyListSort p= sortBy (\(_,a) (_,b)-> compare a b) p

{--polycom :: Num t => (t, Int) -> (t, Int) -> [(t, Int)]
polycom (a,b) (c,d)=if (b:: Int)==(d::Int) then [(a+c,b)] else  [(a,b)]
--}
{- -----------------------------------------------------------------
 - listToTuple
 - -----------------------------------------------------------------
 - it takes a list of tuple and changes it to tuple
 -}

listToTuple :: [(t1, t)] -> (t1, t)
listToTuple [(a,b)]=(a,( b))

{--polylistcom :: [(Int, Int)] -> [(Int, Int)]
polylistcom [(a,b)]=[(a,b)]
polylistcom (p:y:ps)=ys
  where
    z=polycom p y
    ys=if (((( (fst (listToTuple z)):: Int)==( (fst p):: Int)) && (((snd (listToTuple z)) ::Int)==( (snd p)::Int)))) then (z ++ polylistcom (y:ps)) else polylistcom (z++ps)
--}

{- -----------------------------------------------------------------
 - checker
 - -----------------------------------------------------------------
 - it takes a sorted list of tuples and gives back a list with 1st element of tuple aranged according to
 - 2nd elemnt of tuple , 1st element is placed at index equating to 2nd element
 - oyher indexs 0 is returned
 -}

checker :: Num a => Int -> [(a, Int)] -> [a]
checker n []=[]
checker n (x:xs)= if (n::Int) == ((snd x)::Int)
   then ([fst x]++ checker (n+1) (xs)) else [0]++checker (n+1) (x:xs)



{- -----------------------------------------------------------------
- polyToPolyList
- -----------------------------------------------------------------
- takes a poly  and gives back a polylist
-}

polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList p = polyListcheck(PolyList (checker 0 (simplist (polyListSort (polyAltListMaker (polyToPolyAlt (p)))))))

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -test for polyListValue
 - -----------------------------------------------------------------
 - - Function:polyListValue
 - - Test Case Number:1
 - - Input∷polyListValue (PolyList [1,2,0,1]) 2
 - - Expected Output:13
 - - Acutal Output:13

 - - Function:polyListValue
 - - Test Case Number:2
 - - Input:polyListValue (PolyList [9,2,0,2,3]) (-1)
 - - Expected Output:8
 - - Acutal Output:8

 - - Function:polyListValue
 - - Test Case Number:3
 - - Input:polyListValue (PolyList [9,0,0,0,1]) (0)
 - - Expected Output:9
 - - Acutal Output:9

 - -----------------------------------------------------------------
 -test for polyListDegree
 - -----------------------------------------------------------------
 -
 - - Function:polyListDegree
 - - Test Case Number:1
 - - Input∷polyListDegree (PolyList [9,0,0,0,1])
 - - Expected Output:4
 - - Acutal Output:4

 - - Function:polyListDegree
 - - Test Case Number:2
 - - Input:polyListDegree (PolyList [2])
 - - Expected Output:0
 - - Acutal Output:0

 - - Function:polyListDegree
 - - Test Case Number:3
 - - Input:polyListDegree (PolyList [2,32,4,54,64,78,132])
 - - Expected Output:6
 - - Acutal Output:6

 - -----------------------------------------------------------------
 -test for polyListDeriv
 - -----------------------------------------------------------------
 -
 - - Function:polyListDeriv
 - - Test Case Number:1
 - - Input∷polyListDeriv (PolyList [1,2,3,3,4])
 - - Expected Output:PolyList [2,6,9,16]
 - - Acutal Output:PolyList [2,6,9,16]

 - - Function:polyListDeriv
 - - Test Case Number:2
 - - Input:polyListDeriv (PolyList [1,2,0])
 - - Expected Output:PolyList [2,0]
 - - Acutal Output:PolyList [2,0]

 - - Function:polyListDeriv
 - - Test Case Number:3
 - - Input:polyListDeriv (PolyList [1,2,0,-1,-7])
 - - Expected Output:PolyList [2,0,-3,-28]
 - - Acutal Output:PolyList [2,0,-3,-28]

 - -----------------------------------------------------------------
 -test for polyListSum
 - -----------------------------------------------------------------
 -
 - - Function:polyListSum
 - - Test Case Number:1
 - - Input∷polyListSum (PolyList [1,2,3,4]) (PolyList [2,23,4,4])
 - - Expected Output:PolyList [3,25,7,8]
 - - Acutal Output:PolyList [3,25,7,8]

 - - Function:polyListSum
 - - Test Case Number:2
 - - Input:polyListSum (PolyList [1,2,3,4]) (PolyList [2,-3,-2,0])
 - - Expected Output:PolyList [3,-1,1,4]
 - - Acutal Output:PolyList [3,-1,1,4]

 - - Function:polyListSum
 - - Test Case Number:3
 - - Input:polyListSum (PolyList [1,0,-8,4]) (PolyList [2,-3,-2,0])
 - - Expected Output:PolyList [3,-3,-10,4]
 - - Acutal Output:PolyList [3,-3,-10,4]

 - - Function:polyListSum
 - - Test Case Number:4
 - - Input:polyListSum (PolyList [1,2]) (PolyList [1,2,3])
 - - Expected Output:PolyList [2,4,3]
 - - Acutal Output:PolyList [2,4,3]
 - -----------------------------------------------------------------
 -test for polyListProd
 - -----------------------------------------------------------------
 -
 - - Function:polyListProd
 - - Test Case Number:1
 - - Input∷polyListProd (PolyList [1,0,-8,4]) (PolyList [2,-3,-2,0])
 - - Expected Output:PolyList [2,-3,-18,32,4,-8,0]
 - - Acutal Output:PolyList [2,-3,-18,32,4,-8,0]

 - - Function:polyListProd
 - - Test Case Number:2
 - - Input:polyListProd (PolyList [1,0,0,0]) (PolyList [0,-3,0,90])
 - - Expected Output:PolyList [0,3,0,90,0,0,0]
 - - Acutal Output:PolyList [0,3,0,90,0,0,0]

 - - Function:polyListProd
 - - Test Case Number:3
 - - Input:polyListProd (PolyList [1,33,21,-2]) (PolyList [12,-3,-2,90])
 - - Expected Output:PolyList [12,393,151,-63,2934,1894,-180]
 - - Acutal Output:PolyList [12,393,151,-63,2934,1894,-180]

 - - Function:polyListProd
 - - Test Case Number:4
 - - Input:polyListProd (PolyList [1,2]) (PolyList [1,2,3])
 - - Expected Output:PolyList [1,4,7,6]
 - - Acutal Output:PolyList [1,4,7,6]
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 -test for polyListToPoly
 - -----------------------------------------------------------------
 -
 - - Function:polyListToPoly
 - - Test Case Number:1
 - - Input∷polyListToPoly (PolyList [1,2,3,4,9])
 - - Expected Output:Sum (Prod (Coef 9) (Prod X (Prod (Coef 1) (Prod X (Prod (Coef 1) (Prod X (Prod X (Coef 1)))))))) (Sum (Prod (Coef 4) (Prod X (Prod (Coef 1) (Prod X (Prod X (Coef 1)))))) (Sum (Prod (Coef 3) (Prod X (Prod X (Coef 1)))) (Sum (Prod X (Coef 2)) (Sum (Coef 1) (Coef 0)))))
 - - Acutal Output:Sum (Prod (Coef 9) (Prod X (Prod (Coef 1) (Prod X (Prod (Coef 1) (Prod X (Prod X (Coef 1)))))))) (Sum (Prod (Coef 4) (Prod X (Prod (Coef 1) (Prod X (Prod X (Coef 1)))))) (Sum (Prod (Coef 3) (Prod X (Prod X (Coef 1)))) (Sum (Prod X (Coef 2)) (Sum (Coef 1) (Coef 0)))))

 - - Function:polyListToPoly
 - - Test Case Number:2
 - - Input:polyListToPoly (PolyList [1])
 - - Expected Output:Sum (Coef 1) (Coef 0)
 - - Acutal Output:Sum (Coef 1) (Coef 0)

 - - Function:polyListToPoly
 - - Test Case Number:3
 - - Input:polyListToPoly (PolyList [1,0,1])
 - - Expected Output:Sum (Prod (Coef 1) (Prod X (Prod X (Coef 1)))) (Sum (Prod X (Coef 0)) (Sum (Coef 1) (Coef 0)))
 - - Acutal Output:Sum (Prod (Coef 1) (Prod X (Prod X (Coef 1)))) (Sum (Prod X (Coef 0)) (Sum (Coef 1) (Coef 0)))

 - -----------------------------------------------------------------
 -
 -test for polyToPolyList
 - -----------------------------------------------------------------
 -
 - - Function:polyToPolyList
 - - Test Case Number:1
 - - Input∷polyToPolyList (Sum X (Prod X X))
 - - Expected Output:PolyList [0,1,1]
 - - Acutal Output:PolyList [0,1,1]

 - - Function:polyToPolyList
 - - Test Case Number:2
 - - Input:polyToPolyList (Sum X (Prod X (Sum X (Coef 2))))
 - - Expected Output:PolyList [0,3,1]
 - - Acutal Output:PolyList [0,3,1]

 - - Function:polyToPolyList
 - - Test Case Number:3
 - - Input:polyToPolyList (Sum (Coef 4) (Prod X (Prod X (Prod X X))))
 - - Expected Output:PolyList [4,0,0,0,1]
 - - Acutal Output:PolyList [4,0,0,0,1]

 - -----------------------------------------------------------------
 -test for getPolyList
 - -----------------------------------------------------------------
 -
 - - Function:getPolyList
 - - Test Case Number:1
 - - Input∷getPolyList "try.txt"
 - - Expected Output:PolyList [1,2,3,4]
 - - Acutal Output:PolyList [1,2,3,4]

 - - Function:getPolyList
 - - Test Case Number:2
 - - Input:getPolyList "assign4test.txt"
 - - Expected Output:PolyList [1,2,3,5325,5326]
 - - Acutal Output:PolyList [1,2,3,5325,5326]

 - - Function:getPolyList
 - - Test Case Number:3
 - - Input:getPolyList "assign4.txt"
 - - Expected Output:PolyList [1,0,3,0,5326]
 - - Acutal Output:PolyList [1,0,3,0,5326]

 - -----------------------------------------------------------------
 -
       --QuickCheck TEST--

Function:polyListSum
Property:commutative Property =>sum of a and b = sum of b and a
Result:Pass

Function:polyListProd
Property:commutative Property=> product of a and b = product of b and a
Result:Pass

Function:polyListDeriv
Property:length of derivative list is 1 less than orignal list except in case of list with one element
Result:Pass

Function:polyListDegree
Property:degree would always be between 0 and length of listToTuple
Result:Pass

Function:polyListValue
Property:value of polynomial at a value can be found using horners process
Result:Pass

only condition when test fail is when input list is [] or output is not a polynomial
in which case undefined is returned
 -}
