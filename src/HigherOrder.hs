module HigherOrder where

import Prelude hiding (curry, uncurry, (.))
import Basics (plus1, plus1', plus1'', plus2, plus2', plus2'')

{-Now we can pass the plus functions as an *argument* to the map function!
  Functions which take other functions as an argument (i.e. map, zip, fold, etc)
  are so-called "higher-order functions" -}
mapped1  = map plus1   [1,2,3,4,5]
mapped2  = map plus1'  [1,2,3,4,5]
mapped3  = map plus1'' [1,2,3,4,5]
mapped4  = map (+1)    [1,2,3,4,5] --partial evaluation again

zipped1 = zip [1,2,3] ["one", "two", "three"]
--[(1,"one"),(2,"two"),(3,"three")]
--zipWith is like mapping over two lists
zipped2 = zipWith (+) [1,2,3] [4,5,6] --[5,7,9]
-- parenthesis are used to partially apply an infix operator to turn it into a prefix function
zipped3 = zipWith (,) [1,2,3] ["one", "two", "three"]
--(,) :: a -> b -> (a, b) So the comma is a pairing *function*, not just a special syntax!
plusthrees = zipWith (.) [plus1, plus1', plus1''] [plus2, plus2', plus2'']
--(.) :: (b -> c) -> (a -> b) -> a -> c So the dot is a *function*, not just a special syntax!

--Here is how it is actually defined
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = g (f x)
--(.) g f x = f (g x) -- note: This is a compilation error!
prefixcomp = (.) plus1 plus2 3
infixcomp  = plus1 . plus2 $ 3
plusthrees' = zipWith (.) [plus1, plus1', plus1''] [plus2, plus2', plus2'']

reduced1 = foldl (+) 0 [1,2,3,4,5] -- (((((0 + 1) + 2) + 3) + 4) + 5)
reduced2 = foldr (+) 0 [1,2,3,4,5] -- (1 + (2 + (3 + (4 + (5 + 0))))
reduced3 = scanl (+) 0 [1,2,3,4,5] -- [0,1,3,6,10,15]
reduced4 = scanr (+) 0 [1,2,3,4,5] -- [15,14,12,9,5,0]

evens = filter even [1..10] -- [2,4,6,8,10]
odds  = filter odd  [1..10] -- [1,3,5,7,9]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
    where
        less = filter (< x) xs
        equal = filter (== x) xs
        more = filter (> x) xs
        
sorted = quickSort "haskell" -- "aehklls" strings are just lists of chars

collatz :: (Integral a) => a -> a  
collatz 1 = 1  
collatz n  
  | even n =  collatz (n `div` 2) -- The |'s are called guards.  They allow us to define
  | odd n  =  collatz (3*n + 1)   -- a function by cases using predicates.

holdsupto n = all (==1) $ map collatz [1..n]
collatz10000 = holdsupto 10000 -- True

newtonsmethod :: (Ord t, Fractional t) => (t -> t) -> (t -> t) -> t -> t
newtonsmethod f f' x0 =
  let xns = iterate (\xn -> xn - (f xn)/(f' xn)) x0
      pairs = zip xns $ tail xns --successive pairs
      (_,zero):_ = dropWhile (\(xn, xn_1) -> abs (xn - xn_1) > 1.0e-10) pairs in
    zero

f x = x^4 - x^3 - 1
f' x = 4*x^3 - 3*x^2

firstzero = newtonsmethod f f' 1     -- 1.3802775690976141
secondzero = newtonsmethod f f' (-1) -- -0.8191725133961645

-- 'curry' converts a function on pairs to a function of two arguments.
-- By default, all functions in haskell are curried to allow for partial evaluation.
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y =  f (x, y)
-- 'uncurry' converts a function of two arguments to a function on pairs.
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst p) (snd p)
