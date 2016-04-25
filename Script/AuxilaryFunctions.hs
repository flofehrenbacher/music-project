module Script.AuxilaryFunctions where

import Euterpea
-- import HSoM

-- repeat phrase n times
timesM :: Int -> Music a -> Music a
timesM    0      m       =  rest 0
timesM    n      m       = m :+: timesM (n-1) m

-- add duration to list of notes
addDur :: Dur -> [Dur -> Music a] -> Music a
addDur    dur    notes            =  line (map (\f -> f dur) notes)

-- add grace note to one note
graceNote :: Int -> Music Pitch        -> Music Pitch
graceNote     n     (Prim (Note dur p)) = note (dur/8) (trans n p) :+: note (7*dur/8) p
graceNote     n     _                   = error "graceNode only available for single note."

