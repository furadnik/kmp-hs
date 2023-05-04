module Main where

import qualified Kmp (needleToStates)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  if length (Kmp.needleToStates "hello") == 5 then putStrLn "Success" else putStrLn "Not."
