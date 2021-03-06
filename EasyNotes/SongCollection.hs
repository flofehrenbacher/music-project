-- | This module provides access to the songs which can be played with /Easy Notes/
module SongCollection where

import Songs.Fun
import Songs.HaenschenKlein
import Songs.AlleMeineEntchen
import Songs.MadWorld

import Euterpea

-- | A song should be a list of /single/, /sequential/ Music Pitches
type Song = [Music Pitch]

-- | the current collection of songs as tuples of songnames and their corresponding list of Music Pitches
songCollection :: [(String,Song)]
songCollection = [("AlleMeineEntchen",entchenList),
                  ("AllNotes",allNotes),
                  ("HaenschenKlein", haenschenList),
                  ("MadWorld",madList)]

-- | list of all names of the current song collection
allSongs :: [String]
allSongs  =  map fst songCollection

-- | song which contains all notes from C4 to B4
allNotes :: Song
allNotes  = map (pitchToMusic qn) $ map pitch [60..71]