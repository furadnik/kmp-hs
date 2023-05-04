module Kmp where

import Data.Maybe

data State a = State
  { index :: Int,
    go_backward_index :: Int,
    forward_letter :: Maybe a
  }
  deriving (Eq, Show)

-- Simulate one step of the KMP algorithm.
kmpStep states State {forward_letter = Nothing, go_backward_index = b} h = kmpStep states (states !! b) h
kmpStep states State {forward_letter = Just f, index = i, go_backward_index = b} h
  | f == h = states !! (i + 1)
  | i == 0 = head states
  | otherwise = kmpStep states (states !! b) h

-- Check whether the current state is a final state -- it doesn't have a forward letter defined, as it is the end of the needle.
kmpIsFound State {forward_letter = f} = isNothing f

-- Build the automaton -- create a list of states without the `go_backward_index`s and then add them using `buildBackwardIndices`.
-- We initialize the fold of buildBackwardIndices with the first two states, as those must have the `go_backward_index` pointed to the first state.
needleToStates needle = snd $ foldl buildBackwardIndices (head states, [head states, states !! 1]) (tail (tail states))
  where
    states = emptyStates 0 needle
    emptyStates i [] = [State {index = i, go_backward_index = 0, forward_letter = Nothing}]
    emptyStates i (n : ns) = State {index = i, go_backward_index = 0, forward_letter = Just n} : emptyStates (i + 1) ns

-- Add backward indices to the states.
-- The `currentState` is the state that this state's predecessor's `go_backward_index` points to.
-- The `states` is the list of predecessor of the state we are building.
-- The `State` is the state we are building.
buildBackwardIndices (currentState, states) State {index = i, forward_letter = n} = (newState, states ++ [State {index = i, forward_letter = n, go_backward_index = newIndex}])
  where
    State {forward_letter = previousLetter} = states !! (i - 1)
    newState@State {index = newIndex} = kmpStep states currentState $ fromJust previousLetter

-- Find all occurrences of the needle in the haystack.
kmpFind needle = kmpFindWithStates 0 states (head states)
  where
    states = needleToStates needle

kmpFindWithStates _ _ _ [] = []
kmpFindWithStates currentPosition states state (h : hs)
  | kmpIsFound newState = getNeedleStart : kmpFindWithStates nextPosition states newState hs
  | otherwise = kmpFindWithStates nextPosition states newState hs
  where
    nextPosition = currentPosition + 1
    newState = kmpStep states state h
    getNeedleStart = currentPosition - length states + 2
