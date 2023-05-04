module Kmp where

import Data.Maybe

data State a = State
  { index :: Int,
    go_backward_index :: Int,
    forward_letter :: Maybe a
  }
  deriving (Eq, Show)

kmpStep states State {forward_letter = Nothing, go_backward_index = b} h = kmpStep states (states !! b) h
kmpStep states State {forward_letter = Just f, index = i, go_backward_index = b} h
  | f == h = states !! (i + 1)
  | i == 0 = head states
  | otherwise = kmpStep states (states !! b) h

emptyStates i [] = [State {index = i, go_backward_index = 0, forward_letter = Nothing}]
emptyStates i (n : ns) = State {index = i, go_backward_index = 0, forward_letter = Just n} : emptyStates (i + 1) ns

kmpIsFound State {forward_letter = f} = isNothing f

buildBackwardIndices (currentState, states) State {index = i, forward_letter = n} = (newState, states ++ [State {index = i, forward_letter = n, go_backward_index = newIndex}])
  where
    State {forward_letter = previousLetter} = states !! (i - 1)
    newState@State {index = newIndex} = kmpStep states currentState $ fromJust previousLetter

needleToStates needle = snd $ foldl buildBackwardIndices (head states, [head states, states !! 1]) (tail (tail states))
  where
    states = emptyStates 0 needle

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
