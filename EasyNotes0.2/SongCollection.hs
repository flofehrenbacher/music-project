module SongCollection where

import Songs.HelpingFunctions

import Songs.HaenschenKlein
import Songs.AlleMeineEntchen
import Songs.MadWorld

import Euterpea

type Song = [Music Pitch]

songCollection :: [(String,Song)]
songCollection = [("AlleMeineEntchen",entchenList),
                  ("HaenschenKlein", haenschenList),
                  ("AllNotes",allNotes),
                  ("MadWorld",madList)]

allNotes :: [Music Pitch]
allNotes  = map (pitchToMusic qn) $ map pitch [60..71]