module LambdaCalculus where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, ReaderT, runReaderT)


{- The following is a simple lambda calculus data type and interpreter.
This demonstrates a common design pattern: embedded DSL / interpreter.
-}
type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr
  = Val Int
  | Add Expr Expr
  | Var String
  deriving (Show)

eval :: Expr -> ReaderT [(String, Int)] Maybe Int --i.e. eval :: Expr -> Eval Int
eval ex = case ex of

  Val n -> return n

  Add x y -> do
    a <- eval x
    b <- eval y
    return (a+b)

  Var x -> do
    env <- ask
    val <- lift (lookup x env) --Note: (lookup x env) is just a "computation".
    return val
{- This is also our first use of "monad transformers".  From the documentation:
http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html

"A monad transformer makes a new monad out of an existing monad, such that computations
of the old monad may be embedded in the new one. To construct a monad with a desired
set of features, one typically starts with a base monad, such as Identity, [] or IO,
and applies a sequence of monad transformers."
-}

env :: Env
env = [("x", 2), ("y", 5)]

ex1 :: Eval Int
ex1 = eval (Add (Val 2) (Add (Val 1) (Var "x")))

example1, example2 :: Maybe Int
example1 = runReaderT ex1 env
--Just 5
example2 = runReaderT ex1 []
--Nothing
