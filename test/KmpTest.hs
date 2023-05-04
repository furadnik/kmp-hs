module Main where

import qualified Kmp
import qualified System.Exit as Exit
import Test.HUnit

test_needleToStates_length :: Test
test_needleToStates_length = TestCase $ assertEqual "should return 6" 6 $ length $ Kmp.needleToStates "Hello"

test_needleToStates_indices :: Test
test_needleToStates_indices = TestCase $ assertEqual "should return [0..5]" [0 .. 5] $ map (\(Kmp.State {Kmp.index = i}) -> i) $ Kmp.needleToStates "Hello"

test_needleToStates_backward_indices_trivial :: Test
test_needleToStates_backward_indices_trivial = TestCase $ assertEqual "should return [0,0,0,1,2,3]" (0 : 0 : [0 .. 3]) $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "ababa"

test_needleToStates_backward_indices_no_matches :: Test
test_needleToStates_backward_indices_no_matches = TestCase $ assertEqual "should return [0,0,0,0,0,0]" (replicate 6 0) $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "Hello"

test_needleToStates_string_of_same :: Test
test_needleToStates_string_of_same = TestCase $ assertEqual "should return [0,0,1]" [0, 0, 1] $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "aa"

test_needleToStates_backward_indices_duplicates :: Test
test_needleToStates_backward_indices_duplicates = TestCase $ assertEqual "should return [0,0,0,0,0,0,1,2,3,4,5]" [0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5] $ map (\(Kmp.State {Kmp.go_backward_index = i}) -> i) $ Kmp.needleToStates "HelloHello"

test_kmpStep_trivial :: Test
test_kmpStep_trivial = TestCase $ assertEqual "should return State {index = 0}" (states !! 1) $ Kmp.kmpStep states (head states) 'a'
  where
    states =
      [ Kmp.State {Kmp.index = 0, Kmp.go_backward_index = 0, Kmp.forward_letter = Just 'a'},
        Kmp.State {Kmp.index = 1, Kmp.go_backward_index = 0, Kmp.forward_letter = Just 'b'}
      ]

test_kmpFind_trivial :: Test
test_kmpFind_trivial = TestCase $ assertEqual "should return [0,5]" [0, 5] $ Kmp.kmpFind "Hello" "HelloHello"

test_kmpFind_sharing_chars :: Test
test_kmpFind_sharing_chars = TestCase $ assertEqual "should return [0,2,4]" [0, 2, 4] $ Kmp.kmpFind "abab" "abababab"

test_kmpFind_sharing_everything :: Test
test_kmpFind_sharing_everything = TestCase $ assertEqual "should return [0,1,2,3,4]" [0 .. 4] $ Kmp.kmpFind "aa" "aaaaaa"

test_kmpFind_short :: Test
test_kmpFind_short = TestCase $ assertEqual "should return [0,1,2,3,4,5]" [0 .. 5] $ Kmp.kmpFind "a" "aaaaaa"

test_kmpFind_no_find :: Test
test_kmpFind_no_find = TestCase $ assertEqual "should return []" [] $ Kmp.kmpFind "Hello" "Hell no"

tests :: Test
tests =
  TestList
    [ TestLabel "needleToStates length" test_needleToStates_length,
      TestLabel "needleToStates indices" test_needleToStates_indices,
      TestLabel "needleToStates backward" test_needleToStates_backward_indices_trivial,
      TestLabel "needleToStates backward no matches" test_needleToStates_backward_indices_no_matches,
      TestLabel "needleToStates backward duplicates" test_needleToStates_backward_indices_duplicates,
      TestLabel "needleToStates backward string of same" test_needleToStates_string_of_same,
      TestLabel "kmpStep trivial" test_kmpStep_trivial,
      TestLabel "kmpFind trivial" test_kmpFind_trivial,
      TestLabel "kmpFind sharing chars" test_kmpFind_sharing_chars,
      TestLabel "kmpFind sharing everything" test_kmpFind_sharing_everything,
      TestLabel "kmpFind short" test_kmpFind_short,
      TestLabel "kmpFind no find" test_kmpFind_no_find
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
