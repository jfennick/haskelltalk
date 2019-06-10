{-# LANGUAGE TemplateHaskell #-}
module Testing where

import Test.QuickCheck

import           Hedgehog (GenT, Property, forAll, property, withTests, assert, check)
import           Hedgehog.Internal.Property (TestLimit(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{- QuickCheck is the famous property based testing framework for haskell.
The idea is that QuickCheck will randomly create values of a given type which can be tested
with propositions.  Propositions are simply functions of several variables to Bool.
The name of the proposition must start with prop_ . -}

prop_commutativeAdd :: Integer -> Integer -> Bool
prop_commutativeAdd n m = n + m == m + n

checkCommAdd = quickCheck prop_commutativeAdd

{- QuickCheck randomly creates values by calling the arbitrary function defined in then
Arbitrary type class.  So all you have to do to use QuickCheck for your own types is to
make your type an instance of Arbitrary. -}
data Point = MkPoint Int Int deriving (Eq, Show)

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (MkPoint x y)

swapPoint :: Point -> Point
swapPoint (MkPoint x y) = MkPoint y x

prop_swapInvolution :: Point -> Bool
prop_swapInvolution point = swapPoint (swapPoint point) == point

checkInvolution = quickCheck prop_swapInvolution

-- We can run all the tests in a module using the runTests command.
return [] -- This line is template haskell magic.  You can ignore it.
runTests = $quickCheckAll
{-
=== prop_commutativeAdd
+++ OK, passed 100 tests.
=== prop_swapInvolution
+++ OK, passed 100 tests.
True
-}
-------------------------------------------------------------------------------------------
-- There is a new testing framework called hedgehog.  This example is from
-- http://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html
-- Given a 5 liter jug and a 3 liter jug, can you measure 4 liters?

data Step = FillBig
          | FillSmall
          | EmptyBig
          | EmptySmall
          | SmallIntoBig
          | BigIntoSmall
          deriving (Show, Eq, Enum)

data State = State
  { bigJug :: Int
  , smallJug :: Int
  } deriving (Show, Eq)

initial :: State
initial = State { bigJug = 0, smallJug = 0}

-- A simple finite state machine to model the states of the water jugs and their state transitions
fsm :: State -> Step -> State
fsm s FillBig = s { bigJug = 5 }
fsm s FillSmall = s { smallJug = 3 }
fsm s EmptyBig = s { bigJug = 0 }
fsm s EmptySmall = s { smallJug = 0 }
fsm (State big small) SmallIntoBig =
    let big' = min 5 (big + small) in
    State { bigJug = big'
          , smallJug = small - (big' - big) }
fsm (State big small) BigIntoSmall =
    let small' = min 3 (big + small) in
    State { bigJug = big - (small' - small)
          , smallJug = small' }

execute :: [Step] -> State
execute = foldl fsm initial

steps :: Monad m => GenT m [Step]
steps = Gen.list (Range.linear 0 20) (Gen.enum FillBig BigIntoSmall)

prop_solution :: Hedgehog.Property
prop_solution = withTests (TestLimit 50000) . Hedgehog.property $ do
    s <- Hedgehog.forAll steps
    let (State big small) = execute s
    assert $ big /= 4

check_check_1, check_check_2, check_check_3 :: IO (Bool)
check_check_1 = check prop_solution
check_check_2 = check prop_solution
check_check_3 = check prop_solution
{- Now we can run it and find counterexamples to our false proposition, i.e. find examples.
Notice that this library doesn't just print a yes or no answer, it actually prints the
source code you are testing (with line numbers) and with the inputs substituted in.

 check prop_solution
    <interactive> failed after 4691 tests and 6 shrinks.

       ┏━━ .stack-work/intero/intero4061vqe.hs ━━━
    44 ┃ prop_solution :: Property
    45 ┃ prop_solution = withTests (TestLimit 50000) . property $ do
    46 ┃     s <- forAll steps
       ┃     │ [ FillBig
       ┃     │ , BigIntoSmall
       ┃     │ , EmptySmall
       ┃     │ , BigIntoSmall
       ┃     │ , FillBig
       ┃     │ , BigIntoSmall
       ┃     │ ]
    47 ┃     let (State big small) = execute s
    48 ┃     assert $ big /= 4
       ┃     ^^^^^^^^^^^^^^^^^

    This failure can be reproduced by running:
    > recheck (Size 90) (Seed 1547696838921356081 8222902885915333511) <property>
False-
-}
-- The output is color coded when run in a terminal
main = sequence [check_check_1, check_check_2, check_check_3]
