-- | Alle meine Entchen
module Songs.AlleMeineEntchen where

import Songs.Fun

import Euterpea

-- | Melody of /Alle meine Entchen/ put together
entchenMelody :: Music Pitch
entchenMelody =  e1 :+: e2 :+: e3 :+: e4 :+: e5

-- | Melody of /Alle meine Entchen/ as list of primitive values
entchenList :: [Music Pitch]
entchenList = musicToList entchenMelody

-- | Single part of /Alle meine Entchen/'s melody
e1,e2,e3,e4,e5 :: Music Pitch
e1 = addDur qn [c 4, d 4, e 4, f 4]
e2 = g 4 hn :+: g 4 hn
e3 = timesM 4 (a 4 qn) :+: (g 4 wn)
e4 = timesM 4 (f 4 qn) :+: timesM 2 (e 4 hn)
e5 = timesM 4 (g 4 qn) :+: c 4 wn