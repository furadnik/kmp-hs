module Main where

import qualified Kmp
import qualified System.Exit as Exit
import Test.HUnit

test_needleToStates_length :: Test
test_needleToStates_length = TestCase $ assertEqual "should return 5" 5 $ length $ Kmp.needleToStates "Hello"

test_needleToStates_indices :: Test
test_needleToStates_indices = TestCase $ assertEqual "should return [0..4]" [0 .. 4] $ map (\(Kmp.State {Kmp.index = i}) -> i) $ Kmp.needleToStates "Hello"

test_needleToStates_backward_indices_trivial :: Test
test_needleToStates_backward_indices_trivial = TestCase $ assertEqual "should return [0,0,1,2,3]" [0, 0, 1, 2, 3] $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "ababa"

test_needleToStates_backward_indices_no_matches :: Test
test_needleToStates_backward_indices_no_matches = TestCase $ assertEqual "should return [0,0,0,0,0]" [0, 0, 0, 0, 0] $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "Hello"

test_needleToStates_backward_indices_duplicates :: Test
test_needleToStates_backward_indices_duplicates = TestCase $ assertEqual "should return [0,0,0,0,0,1,2,3,4,5]" [0, 0, 0, 0, 0, 1, 2, 3, 4, 5] $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "HelloHello"

test_kmpStep_trivial :: Test
test_kmpStep_trivial = TestCase $ assertEqual "should return State {index = 0}" (states !! 1) $ Kmp.kmpStep states (head states) 'a'
  where
    states =
      [ Kmp.State {Kmp.index = 0, Kmp.go_backward_index = 0, Kmp.forward_letter = 'a'},
        Kmp.State {Kmp.index = 1, Kmp.go_backward_index = 0, Kmp.forward_letter = 'b'}
      ]

tests :: Test
tests =
  TestList
    [ TestLabel "needleToStates length" test_needleToStates_length,
      TestLabel "needleToStates indices" test_needleToStates_indices,
      TestLabel "needleToStates backward" test_needleToStates_backward_indices_trivial,
      TestLabel "needleToStates backward no matches" test_needleToStates_backward_indices_no_matches,
      TestLabel "needleToStates backward duplicates" test_needleToStates_backward_indices_duplicates,
      TestLabel "kmpStep trivial" test_kmpStep_trivial
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
