-- | Hänschen Klein
module Songs.HaenschenKlein where

import Songs.Fun

import Euterpea

-- | Melody of /Haenschen Klein/ put together
haenschenMelody :: Music Pitch
haenschenMelody = h1 :+: h2 :+: h1 :+: h3 :+: h4 :+: h5 :+: h1 :+: h3

-- | Melody of /Haenschen Klein/ as list of primitive values
haenschenList :: [Music Pitch]
haenschenList = musicToList haenschenMelody

-- | Single part of /Haenschen Klein/'s melody
h1,h2,h3,h4,h5 :: Music Pitch
h1 = a 4 en :+: fs 4 en :+: fs 4 qn :+: g 4 en :+: e 4 en :+: e 4 qn
h2 = addDur en [d 4, e 4, fs 4, g 4, a 4, a 4] :+: a 4 qn
h3 = addDur en [d 4, fs 4, a 4, a 4] :+: d 4 hn
h4 = timesM 5 (e 4 en) :+: fs 4 en :+: g 4 qn
h5 = timesM 5 (fs 4 en) :+: g 4 en :+: a 4 qn