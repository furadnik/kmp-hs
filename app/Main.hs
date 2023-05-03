module Main where

import qualified Kmp (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Kmp.someFunc
