module Kmp where

data State a = State
  { index :: Int,
    go_backward_index :: Int,
    forward_letter :: a
  }
  deriving (Eq, Show)

kmpStep states State {forward_letter = f, index = i, go_backward_index = b} h
  | f == h = states !! (i + 1)
  | i == 0 = head states
  | otherwise = kmpStep states (states !! b) h

emptyStates _ [] = []
emptyStates i (n : ns) = State {index = i, go_backward_index = 0, forward_letter = n} : emptyStates (i + 1) ns

buildBackwardIndices (currentState, states) State {index = i, forward_letter = n} = (newState, states ++ [State {index = i, forward_letter = n, go_backward_index = newIndex}])
  where
    newState@State {index = newIndex} = kmpStep states currentState n

needleToStates needle = snd $ foldl buildBackwardIndices (head states, [head states]) (tail states)
  where
    states = emptyStates 0 needle
