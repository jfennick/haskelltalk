module Main where

--Just like other languages, execution starts with the function called "main"
main :: IO ()
main = helloWorld

helloWorld :: IO ()
helloWorld = do
  input <- getLine
  putStrLn ("hello " ++ input)

--ignore everything below here for now
print123 :: [IO ()] --this is a list of IO "actions"
print123 = [putStrLn "1", putStrLn "2", putStrLn "3"]
printabc :: [IO ()]
printabc = [putStrLn "a", putStrLn "b", putStrLn "c"]
printboth :: IO [()] --this is a single IO action with does all of the actions in the list
printboth = sequence $ print123 ++ reverse printabc
