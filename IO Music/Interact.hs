module Interact where

import Euterpea

--readMaybe?????
stringToPitchClass :: String -> (Octave -> Dur -> Music Pitch)
stringToPitchClass    "c"    =  c
stringToPitchClass    "d"    =  d
stringToPitchClass    "e"    =  e
stringToPitchClass    "f"    =  f
stringToPitchClass    "g"    =  g
stringToPitchClass    "a"    =  a
stringToPitchClass    "b"    =  b
stringToPitchClass    _      = error "note does not exist"

-- with do-notation
main :: IO ()
main  = do
            putStrLn "which note should be played?"
            l <- getLine
            play $ (stringToPitchClass l) 4 wn

main' :: IO ()
main' = putStrLn "enter note to be played: " >> getLine >>= \l -> play $ (stringToPitchClass l) 4 wn