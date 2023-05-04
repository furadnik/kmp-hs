module Main where

import qualified Kmp
import System.Environment

main :: IO ()
main = do
  haystack <- getContents
  needles <- getArgs
  mapM_ (\needle -> mapM_ print $ Kmp.kmpFind needle haystack) needles
