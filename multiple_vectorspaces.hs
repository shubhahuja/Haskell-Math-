
import Data.List


newtype Vector2 a = Vector2 (a,a)
  deriving (Show,Eq)
newtype Vector3 a = Vector3 (a,a,a)
  deriving (Show,Eq)
newtype Vector4 a = Vector4 (a,a,a,a)
  deriving (Show,Eq)

class VectorSpace v where
  vecZero       :: (Num a) => v a
  vecSum        :: (Num a) => v a -> v a -> v a
  vecScalarProd :: (Num a) => a -> v a -> v a
  vecMagnitude  :: (Floating a) => v a -> a
  vecInnerProd  :: (Num a) => v a -> v a-> a
  
instance VectorSpace Vector2 where
  vecZero       = Vector2 (0,0)
  vecSum (Vector2 (a,b)) (Vector2 (a2,b2))= Vector2 (a+a2,b+b2)
  vecScalarProd c (Vector2 (a2,b2))= Vector2 (c*a2,c*b2)
  vecMagnitude  (Vector2 (a,b))= sqrt(a**2+b**2)
  vecInnerProd  (Vector2 (a,b)) (Vector2 (a2,b2))= a*a2+b*b2


instance VectorSpace Vector3 where
  vecZero       = Vector3 (0,0,0)
  vecSum (Vector3 (a,b,c)) (Vector3 (a2,b2,c2))= Vector3 (a+a2,b+b2,c+c2)
  vecScalarProd c (Vector3 (a2,b2,c2))= Vector3 (c*a2,c*b2,c*c2)
  vecMagnitude  (Vector3 (a,b,c))= sqrt(a**2+b**2+c**2)
  vecInnerProd  (Vector3 (a,b,c)) (Vector3 (a2,b2,c2))= a*a2+b*b2+c*c2



instance VectorSpace Vector4 where
  vecZero       = Vector4 (0,0,0,0)
  vecSum (Vector4 (a,b,c,d)) (Vector4 (a2,b2,c2,d2))= Vector4 (a+a2,b+b2,c+c2,d+d2)
  vecScalarProd c (Vector4 (a2,b2,c2,d2))= Vector4 (c*a2,c*b2,c*c2,c*d2)
  vecMagnitude  (Vector4 (a,b,c,d))= sqrt(a**2+b**2+c**2+d**d)
  vecInnerProd  (Vector4 (a,b,c,d)) (Vector4 (a2,b2,c2,d2))= a*a2+b*b2+c*c2+d*d2
