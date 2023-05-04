# kmp-hs
Implementation of the KMP algorithm in Haskell

## Kmp module

The `Kmp` module exports functions for searching in text.

For basic usage of the module, there is the function 
```haskell
Kmp.kmpFind :: Eq a => [a] -> [a] -> [Int]
```
This function takes a pattern ("needle") and a text ("haystack"), and returns a
list of occurences of the needle in the haystack. Specifically, it returns a
list of indices of the beginnings of the needle.

For example:
```haskell
import qualified Kmp

kmpFind "Hello" "HelloHello" -- returns [0, 5]
kmpFind "abab" "ababab" -- returns [0, 2]
kmpFind "a" "aaaa" -- returns [0..3]
```

For advanced usage, there are also the functions `Kmp.kmpFindWithStates`, which
takes states as an argument instead of the needle. Further, the function
`needleToStates` generates the automaton for searching the haystack and
`kmpStep` simulates one step of that automaton.

For further info about the functions, look inside `src/Kmp.hs`.
