{-# LANGUAGE BangPatterns #-}

module LazyEvaluation where

import Basics --import everything (useful for autocomplete)
import Basics (fib) -- can also import just the functions listed here

import Control.Applicative (Alternative)
import Data.Array (array, (!))

{- In a lazy language, ... e ... is the same as
let x = e in ... x ...

This is NOT true in a strict language.  For example,
if c then (error "BOO!") else 0
is NOT the same as
let x = error "BOO!" --this will immediately evaluate error!
    in  if c then x else 0

Lazy evaluation lets us define our own control structures. (No macros needed!) -}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
{- In Haskell, evaluation is driven by pattern matching on type constructors.
  Since we are only pattern matching on the Bool constructors, we can choose to
  evaluate x or y at the use site (or neither). -}
f x = if' (x < 0) (error "x must be >= 0") "OK!"

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x,y,z) =  f x y z

-- Since if' is just a regular function, we can map our new control structure over some code!
results = map (uncurry3 if') [(True,"Hwat!?", error ""), (True, "Okay!", error ""),
                              (True, "Yeah!", error ""), (False, error "Oh no!" , error "Oh no!")]
goodresults = take 3 results -- ["Hwat!?","Okay!","Yeah!"]

{- Lazy evaluation helps with code reuse.  In a strict language, this would not short circuit properly!
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

Here is another example of code reuse:

input <- read <$> getLine -- get some user input
let x = foo input -- suppose foo is a function which takes a long time to calculate
bar x -- in a lazy language, x doesn't get evaluated until it is needed, i.e. in the body of bar

bar x = do -- still haven't evaluated x yet
    b <- someIOPredicate -- this returns true or false depending on user input
    when b (print x) -- now we evaluate x (and hence call foo), but only if b is true
Note: when is just a regular function, not some special syntax.

In a strict language, we can emulate this by manually inlining.
bar input = do
    b <- someIOPredicate
    when b (print (foo input))  --manually inline foo input call
But manually copy & pasteing code is bad!  Lazy evaluation lets us write composable code!

Lazy evaluation also allows us to create infinite data structures. -}
fives = take 10 (repeat 5)
--[5,5,5,5,5,5,5,5,5,5]
splitfives = (head fives, tail fives)
-- (5,[5,5,5,5,5,5,5,5,5]) --beware! head [] is a runtime error

-- the fibonacci numbers, all of them
fibs, fibs' :: Num a => [a]
fibs' = map fib [0..]
fibs = 1:1:zipWith (+) fibs (tail fibs) -- lazy infinite list
fibs10 = take 10 fibs --[1,1,2,3,5,8,13,21,34,55]

--prime sieve
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
primes10  = take 10 primes --[2,3,5,7,11,13,17,19,23,29]

{- On the topic of "space leaks":  There are times where your program may use more memory
than you expect.  This is usually caused by unintentionally holding onto a reference
such that your data structure cannot be immediately garbage collected.  Note that this
behavior isn't any worse than if you did the same thing in a strict language! -}

leaky = do
  let bigstring = show (2^(2^27)) --40403563 = length $ bigstring ()
  putStrLn $ take 1 $ drop 1000000 bigstring
  putStrLn $ take 1 bigstring
{- Since bigstring is used in the second line, we cannot garbage collect the first
1000000 entries until the second line! We are unintentionally "holding onto head". -}

noleaky = do
  let bigstring = show (2^(2^27)) --40403563 = length $ bigstring ()
  let h = take 1 $ bigstring -- Get a separate reference to the first element
  putStrLn $ take 1 $ drop 1000000 bigstring -- and now we can garbage collect :)
  putStrLn $ h

-- The other common case where space leaks occur is with a lazy accumulator
listoflists = [[0..9],[10..19],[20..29]]
lazyaccumulator = foldl (++) [] listoflists

strictconcat !x y = x ++ y -- The ! is called a "bang pattern" and forces evaluation
strictaccumulator = foldl strictconcat [] listoflists

-- Lazy evaluation allows us to use the technique of dynamic programming to solve the knapsack problem.
-- https://en.wikipedia.org/wiki/Knapsack_problem
knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
     -- The array function creates the array m
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)]
     -- But due to lazy evaluation we can refer to m inside of the expression that is creating it.
     -- Note the lack of explicit looping or mutation!
example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20


--recursive data types
data Tree a = Leaf a | Bin (Tree a) (Tree a)

--This function replaces each value of a leaf in a tree with the minimum value within that tree.
repmin :: Ord a => Tree a -> Tree a
repmin t = r
      where (r, m) = repmin' t m
            repmin' (Leaf i) m = (Leaf m, i)
            repmin' (Bin l r) m = let (lt, lm) = repmin' l m
                                      (rt, rm) = repmin' r m
                                   in (Bin lt rt, lm `min` rm)

--interesting example
guardId :: Alternative f => (a -> Bool) -> f a -> a -> f a
guardId pred alt x = if pred x then pure x else alt

--foo lazily refers to itself, so when we finally force it via show, it will call itself recursively until you satisfy pred
foo = fmap read getLine >>= guardId (0<) (putStrLn "Please enter a number greater than 0" >> foo)

positive :: IO ()
positive = foo >>= putStrLn . (++ " is a positive number!") . show
