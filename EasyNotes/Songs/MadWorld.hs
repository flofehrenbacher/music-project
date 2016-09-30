-- | Mad World
module Songs.MadWorld where

import Songs.Fun

import Euterpea

-- | Melody of /Mad World/ put together
madMelody :: Music Pitch
madMelody = mi :+: timesM 4 m12 :+: m3' :+: m4 :+: m3 :+: m5 :+: m3 :+: m4 :+: m3 :+: m5

-- | Melody of /Mad World/ as list of primitive values
madList :: [Music Pitch]
madList = musicToList madMelody

-- | Single part of /Mad World/'s melody
m1,m2,m12,m3,m4,m5,mi,mi1,mi2,m3' :: Music Pitch

-- intro                                                        
mi1 = addDur en [af 4, c 5, g 4, af 4, f 4, g 4, ef 4, d 4] :+: rest ((7/8) * wn)
mi2 = af 4 qn :+: addDur en [c 5, g 4, af 4, f 4, g 4, af 4] :+: bf 4 ((9/8) * wn)

mi = mi1 :+: mi2

-- verse
m1 = rest qn :+: addDur sn [f 4, rest, f 4, rest, af 4, rest, af 4, rest, f 4, rest, f 4, rest] :+: c 5 en :+: c 5 qn :+: c 5 qn :+: af 4 en :+: rest qn
m2 = bf 4 en :+: bf 4 qn :+: bf 4 qn :+: g 4 en :+: rest qn :+: bf 4 en :+: bf 4 qn :+: bf 4 qn :+: addDur en [af 4, g 4, f 4]

m12 = m1 :+: m2

-- refrain
m3 = (f 4 en) :+: rest qn :+: addDur en [f 4, af 4, af 4, c 5, c 5]
m3' = rest qn :+: addDur en [f 4, f 4, af 4, af 4, c 5, c 5]
m4 = addDur en [d 5, bf 4] :+: rest en :+: addDur en [bf 4, d 5, d 5, bf 4, bf 4]
m5 = addDur en [d 5, bf 4, bf 4, bf 4, d 5, d 5, bf 4, bf 4]