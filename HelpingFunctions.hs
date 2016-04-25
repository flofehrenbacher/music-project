module HelpingFunctions where

import Euterpea
import HSoM

-- transforms phrase of music into list of primitives
musicToList :: Music a      -> [Music a]
musicToList   m@(Prim _)           =  [m]
musicToList  (m@(Prim _) :+: rest) =  m : (musicToList rest)
musicToList  (m          :+: rest) =  (musicToList m) ++ (musicToList rest)

toAkkord :: [(Octave -> Dur -> Music Pitch, Int)] -> Dur -> Music Pitch
toAkkord    []               dur = rest 0
toAkkord    ((p,o):accord)   dur = (p o dur) :=: toAkkord accord dur

-- transforms a given Pitch to playable Music Pitch
pitchToMusic :: Dur ->    Pitch   -> Music Pitch
pitchToMusic    d         p       =  Prim (Note d p)

-- pentatonic
penta :: PitchClass -> [Pitch]
penta    c     =  [start, trans 3 start, trans 5 start, trans 7 start, trans 10 start]
                  where start = (c,3)

-- transforms a given Pitch to its major scale
majorScale :: Pitch -> Music Pitch
majorScale    p     = line $ map (pitchToMusic qn) $ (toScale p)

toScale :: Pitch -> [Pitch]
toScale    p     = [p, trans 2 p, (trans 4 p),(trans 5 p),(trans 7 p),(trans 9 p),(trans 11 p),(trans 12 p)]
                ++ reverse [p,(trans 2 p),(trans 4 p),(trans 5 p),(trans 7 p),(trans 9 p),(trans 11 p),(trans 12 p)]

