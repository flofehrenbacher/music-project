module SongCollection where

import Songs.HelpingFunctions

import Songs.HaenschenKlein
import Songs.AlleMeineEntchen
import Songs.MadWorld

import Euterpea

song :: [Music Pitch]
song = allNotes

allNotes :: [Music Pitch]
allNotes  = map (pitchToMusic qn) $ map pitch [60..71]