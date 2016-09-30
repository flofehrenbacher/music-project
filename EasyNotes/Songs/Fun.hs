-- | The functions defined in this module make it easier to transcribe songs into code
module Songs.Fun where

import Euterpea

-- | Repeats phrase n times
timesM :: Int -> Music a -> Music a
timesM    0      m       =  rest 0
timesM    n      m       = m :+: timesM (n-1) m

-- | Adds duration to list of notes
addDur :: Dur -> [Dur -> Music a] -> Music a
addDur    dur    notes            =  line (map (\f -> f dur) notes)

-- | Transforms a phrase of music into list of primitives
musicToList :: Music a      -> [Music a]
musicToList   m@(Prim _)           =  [m]
musicToList  (m@(Prim _) :+: rest) =  m : (musicToList rest)
musicToList  (m          :+: rest) =  (musicToList m) ++ (musicToList rest)

-- | Transforms a given Pitch to a playable Music Pitch
pitchToMusic :: Dur ->    Pitch   -> Music Pitch
pitchToMusic    d         p       =  Prim (Note d p)