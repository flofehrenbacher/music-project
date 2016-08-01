module GuitarSolo where

---- :=:,:+: have precedence 5 and are right-associative ----
-- for markovChain import Data.MarkovChain and System.Random to use mkStdGen

import HelpingFunctions

import Euterpea
import HSoM
import Data.MarkovChain
import System.Random

-- AcousticGuitarSteel

main = play $ mainSolo :=: mainAccompany

mainSolo = instrument AcousticGuitarSteel $ line $ run 1 solo 0 (mkStdGen 1)
mainAccompany = forever $ instrument AcousticGuitarSteel $ 
                toAkkord gAkkord wn :+:
                toAkkord dAkkord wn :+:
                toAkkord cAkkord wn :+:
                toAkkord dAkkord wn

solo :: [Music Pitch]
solo = map (pitchToMusic en) (penta E) ++ map (pitchToMusic en) (reverse (penta E))
     ++ map (pitchToMusic en) (toScale (G,3))


gAkkord, cAkkord, dAkkord :: [(Octave -> Dur -> Music Pitch, Int)]
gAkkord = [(g,2), (b,2), (d,3), (g,3), (d,4), (g,4)]
dAkkord = [(d,3), (a,3), (d,4), (fs,4)]
cAkkord = [(c,3), (e,3), (g,3), (c,4), (e,4)]