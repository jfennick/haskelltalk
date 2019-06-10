module Accelerate where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
--import Data.Array.Accelerate.LLVM.PTX     as GPU

import Control.Exception
import Data.Time

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

xs = fromList (Z:.75000000) [0..]   :: Vector Float
ys = fromList (Z:.75000000) [1,3..] :: Vector Float

cpuresult = CPU.run $ dotp (use xs) (use ys)
--gpuresult = GPU.run $ dotp (use xs) (use ys)

main = do
    start1 <- getCurrentTime
    putStrLn $ show cpuresult
    end1 <- getCurrentTime
    print (diffUTCTime end1 start1)
    
    start2 <- getCurrentTime
    --putStrLn $ show gpuresult
    end2 <- getCurrentTime
    print (diffUTCTime end2 start2)
{-
Scalar Z [1.8553243e22]
16.224907s
Scalar Z [1.8552674e22]
0.458328s
-}
