module Main where

import Challenges

main :: IO ()
main = do
    putStrLn "please help me"
    x <- createWordSearch ["HASKELL","STRING","STACK","MAIN","METHOD"] 0.1
    print x
