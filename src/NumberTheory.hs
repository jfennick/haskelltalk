module NumberTheory where

import Math.NumberTheory.Primes.Factorisation (factorise, smallFactors)
import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Euclidean (coprime)
import qualified Math.NumberTheory.Quadratic.GaussianIntegers as G
import Math.NumberTheory.Moduli (chineseRemainder)

-- Find the prime factorization of a number using the elliptic curve method described here:
-- http://programmingpraxis.com/2010/04/23/modern-elliptic-curve-factorization-part-1/
-- http://programmingpraxis.com/2010/04/27/modern-elliptic-curve-factorization-part-2/
factorization1 = factorise 123456789 -- [(3,2),(3607,1),(3803,1)]
factorization2 = factorise 987654321 -- [(3,2),(17,2),(379721,1)]

-- We can do partial factorization
smallfactors1 = smallFactors 1000 123456789 -- ([(3,2)],Just 13717421)
smallfactors2 = smallFactors 1000 987654321 --([(3,2),(17,2),(379721,1)],Nothing)

tot1, tot2 :: Integer
{- Note: If we omit this type signature, the compiler will complain about an
ambiguous type variable.  This is because numeric literals are polymorphic,
and so the compiler can't determine if 123456789 is an Int or an Integer,
and thus it can't determine which implementation of the corresponding
type class to use.  This is precisely the phenomenon mentioned in Note 3 of
Category.hs  Again, the solution is simply to supply a type signature.-}
tot1 = totient 123456789 -- 82260072
tot2 = totient 987654321 -- 619703040

thegcd :: Integer
thegcd = gcd 123456789 987654321 -- 9 = 3^2

iscoprime :: Bool
iscoprime = coprime (123456789 :: Integer) (987654321 :: Integer) -- False

gprimes10 :: [G.GaussianInteger]
gprimes10 = take 10 G.primes -- [1+ι,3,1+2*ι,2+ι,7,11,3+2*ι,2+3*ι,4+ι,1+4*ι]

-- Can we find an integer n such that n = 1 mod 2, n = 2 mod 3, etc.?
solution = chineseRemainder [(1,2), (2,3), (3,5), (4,7), (5,11)] -- Just 1523
