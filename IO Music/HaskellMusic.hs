module HaskellMusic where

-- aufgabe:
-- billigkeyboard: töne eingeben und return drücken -> ton ausgeben -> falsche eingabe error
-- opengl fenster öffnen und tastatureingabe über opengl steuern
-- optional: R -> alle töne bisher nochmal

-- :=:,:+: have precedence 5 and are right-associative ----
-- for markovChain import Data.MarkovChain and System.Random to use mkStdGen
-- run (prediction Context) (training Sequence) (index to start random walk)
-- mkStdGen erzeugt zufallsgenerator

import Text.Read
import Data.Char

import HelpingFunctions
import Script.AuxilaryFunctions

import Euterpea
import HSoM
import Data.MarkovChain
import System.Random

-- Types
entchenMelody, haenschenMelody, madMelody :: Music Pitch
entchenList, haenschenList, madList :: [Music Pitch]

-- play melody of song randomly
playRandom  song  = play $ line $ run 4 song 0 (mkStdGen 2)

{------------------------ Alle meine Entchen ----------------------------}

e1,e2,e3,e4,e5 :: Music Pitch
e1 = addDur qn [c 4, d 4, e 4, f 4]
e2 = g 4 hn :+: g 4 hn
e3 = timesM 4 (a 4 qn) :+: (g 4 wn)
e4 = timesM 4 (f 4 qn) :+: timesM 2 (e 4 hn)
e5 = timesM 4 (g 4 qn) :+: c 4 wn

entchenMelody =  e1 :+: timesM 2 e2 :+: e3 :+: e4 :+: e5

entchenList = musicToList entchenMelody

{------------------------- Haenschen Klein --------------------------------}

h1,h2,h3,h4,h5 :: Music Pitch
h1 = a 4 en :+: fs 4 en :+: fs 4 qn :+: g 4 en :+: e 4 en :+: e 4 qn
h2 = addDur en [d 4, e 4, fs 4, g 4, a 4, a 4] :+: a 4 qn
h3 = addDur en [d 4, fs 4, a 4, a 4] :+: d 4 hn
h4 = timesM 5 (e 4 en) :+: fs 4 en :+: g 4 qn
h5 = timesM 5 (fs 4 en) :+: g 4 en :+: a 4 qn

haenschenMelody = h1 :+: h2 :+: h1 :+: h3 :+: h4 :+: h5 :+: h1 :+: h3

haenschenList = musicToList haenschenMelody

{--------------------------- MAD WORLD ----------------------------------}

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

-- melody put together
madMelody = mi :+: timesM 4 m12 :+: m3' :+: m4 :+: m3 :+: m5 :+: m3 :+: m4 :+: m3 :+: m5

-- converted to list
madList = musicToList madMelody

-- komisch nach erstem Durchlauf wegen Tempo????
madWorld = let t = (88/120)
           in tempo t madMelody
