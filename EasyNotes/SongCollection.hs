module SongCollection where

import Songs.HelpingFunctions

import Songs.HaenschenKlein
import Songs.AlleMeineEntchen
import Songs.MadWorld

import Euterpea

type Song = [Music Pitch]

songCollection :: [(String,Song)]
songCollection = [("AlleMeineEntchen",entchenList),
                  ("AllNotes",allNotes),
                  ("HaenschenKlein", haenschenList),
                  ("MadWorld",madList)]

allSongs :: [String]
allSongs  =  map fst songCollection

allNotes :: Song
allNotes  = map (pitchToMusic qn) $ map pitch [60..71]