-- Neural Networks as a DAMG catamorphism.
module CataNN

import Data.Vect

swapVect : (m : Nat) -> Vect (m + n) a -> Vect (n + m) a
swapVect m xs = let (ys, zs) = splitAt m xs in 
                    zs ++ ys 

vComp : (Vect m a -> Vect n a)
     -> (Vect p a -> Vect q a)
     -> Vect (m + p) a
     -> Vect (n + q) a
vComp {m} f g xs = let (ys, zs) = splitAt m xs in
                       f ys ++ g zs

hComp : (Vect m a -> Vect n a)
     -> (Vect n a -> Vect p a)
     -> Vect m a
     -> Vect p a
hComp f g xs = g (f xs) 

dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct xs ys = foldl (+) 0 (zipWith (*) xs ys)

data NN : Nat -> Nat -> Type -> Type where
  Vert : (Vect m a) -> (a -> a) -> (n : Nat) -> NN m n a
  Edge : NN (S Z) (S Z) a
  Empty : NN Z Z a
  Swap : (m : Nat) -> NN (m + n) (n + m) a
  Beside : NN m n a -> NN p q a -> NN (m + p) (n + q) a
  Before : NN m n a -> NN n p a -> NN m p a

cataNN : Num a => NN m n a -> Vect m a -> Vect n a
cataNN (Vert ws f n) = (\xs => replicate n (f (xs `dotProduct` ws)))
cataNN Edge          = (\xs => xs)
cataNN Empty         = (\xs => xs)
cataNN (Swap m)      = swapVect m
cataNN (Beside x y)  = vComp (cataNN x) (cataNN y)
cataNN (Before x y)  = hComp (cataNN x) (cataNN y)


