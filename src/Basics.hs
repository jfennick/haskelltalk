module Basics where

x=2 -- Unlike most languages,  = means equals, it does NOT mean assignment! data is "immutable".
--x=3 -- You cannot define 2=3. This is a compile error! :)

plus :: Integer -> Integer -> Integer -- This is a type signature. :D
-- Type signatures make code much easier to read. :)
plus   x y = x + y
-- More importantly, the types are checked by the compiler, so
-- err = plus 1 (2.5 :: Double) -- is a compile error! :D
{-  Basics.hs:10:15: error:
    • Couldn't match expected type ‘Integer’ with actual type ‘Double’
    • In the second argument of ‘plus’, namely ‘(2.5 :: Double)’
      In the expression: plus 1 (2.5 :: Double)
      In an equation for ‘err’: err = plus 1 (2.5 :: Double)
-}
--This is what is meant by "purely functional".  It allows you to reason locally about your code.

plusDouble :: Double -> Double -> Double
plusDouble x y = x + y
-- similarly, this is a compile error
-- err2 = plusDouble (1 :: Integer) 2.5
{-Some languages let you implicitly convert between types.  This is very bad.  Notably,
conversion from a 64 bit floating point value to a 16 bit signed integer value caused the
Ariane 5 rocket to self-destruct on it's first test flight.  Haskell doesn't allow this!
-}
okay = plusDouble (fromIntegral (1 :: Integer)) 2.5

plus'  x y = sum where -- where clause can be used to create local bindings
  x' = x; y' = y -- can use semicolons or newlines to separate bindings.
  -- Note that Haskell has significant whitespace (like python)
  sum = x' + y' -- so this sum must be indented to the same column as x'
plus'' x y = let x' = x; y' = y -- let can also be used to create local bindings
                 sum = x' + y' in -- Note that this x', y', and sum are
               sum -- not the same as the x', y', and sum in plus'
three  = plus 1 2
three' = 1 `plus` 2 --backticks `` are used to turn prefix functions into infix operators

{- Haskell has type inference, so we can omit the type and have the compiler fill in the type for us!  In Emacs, press Ctrl-u Ctrl-c Ctrl-t with your cursor on the plus function to get
plus :: Num a => a -> a -> a
The letter a is a type variable.  Functions without concrete types are called polymorphic.
We see that the plus function works on any number, not just integers! -}

plus1  y = plus 1 y
plus1'   = (\y -> plus 1 y) --anonymous functions! aka lambdas :)
plus1''  = plus 1  --whitespace is partial evaluation! :)
-- Note that this is an example of so-called "point-free style".  No need to write the y!

plus2  y = plus1 (plus1 y) --by default, functions associate from the left
--i.e. plus1 plus1 y means (plus1 plus1) y which is a compile error
plus2' y = plus1 $ plus 1 y -- the $ function is used to remove parentheses
plus2''  = plus1 . plus1  -- the . function is function composition! :)

--nth fibonacci number
fib 0 = 1  --define a function by cases
fib 1 = 1  --cases are tried in order
fib n = fib (n-1) + fib (n-2) --we can define a function recursively :)
--TODO: add guard for negative n

--lists, tuples, and destructuring
squares15 = [x^2 | x <- [1..5]] --[1,4,9,16,25]

squared1 pair = (fst pair)^2 + (snd pair)^2 --no destructuring
squares15'  = [squared1 (x,x) | x <- [1..5]]

squared2 (x,y) = x^2 + y^2 --destructuring :)  Note that the top-level x is not the same as this x which only exists in the scope of the function body
squares15'' = [squared2 (x,x) | x <- [1..5]]

--We can use list comprehensions like Python
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
--[(3,4,5),(6,8,10)] --note: can use value of c in b, and values of b & c in a
