{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- This module demonstrates one of the use cases of dependent types.
Note: this module works with GHC 7.10.  GHC 8.0 introduced the TypeInType language extension
which makes writing code which simulates dependent types easier, and full-blown
dependent types are coming soon (hopefully). -}

module SizeIndexedVectors where

data Nat = Z | S Nat deriving (Eq, Show)

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four

-- Here we use a generalized algebraic data type for our vector.
data Vec :: Nat -> * -> * where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance Show a => Show (Vec n a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

class FromList n where
  fromList :: [a] -> Vec n a

instance FromList Z where
  fromList [] = Nil

instance FromList n => FromList (S n) where
  fromList (x:xs) = Cons x $ fromList xs

lengthVec :: Vec n a -> Nat
lengthVec Nil = Z
lengthVec (Cons x xs) = S (lengthVec xs)

zipVec :: Vec n a -> Vec n b -> Vec n (a,b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x,y) (zipVec xs ys)

vec4 :: Vec Four Int
vec4 = fromList [0, 1, 2, 3]

vec5 :: Vec Five Int
vec5 = fromList [5, 6, 7, 8, 9]

example1 :: Nat
example1 = lengthVec vec4
-- S (S (S (S Z)))

example2 :: Vec Four (Int, Int)
example2 = zipVec vec4 vec4
-- Cons (0,0) (Cons (1,1) (Cons (2,2) (Cons (3,3) (Nil))))

instance Functor (Vec n) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

double4 = fmap (*2) vec4
double5 = fmap (*2) vec5
