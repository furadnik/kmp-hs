module Main where

import qualified Kmp

main :: IO ()
main = do
  print $ Kmp.needleToStates "a"
  print $ Kmp.kmpFind "aa" "aaaa"
