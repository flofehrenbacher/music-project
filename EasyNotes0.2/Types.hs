module Types where

import Euterpea
import Data.IORef
import Graphics.UI.GLUT
import Data.Time.Clock

type GreenPlace = Maybe GLfloat
type RedPlace = Maybe GLfloat
type LastNote = Maybe PitchClass
type SongInfo = Maybe (PitchClass, [Music Pitch])
type NotePlace = GLfloat
type MyTime = IORef UTCTime
data DisplayInfo = DisplayInfo {greenPlace :: GreenPlace, redPlace :: RedPlace, songInfo :: SongInfo, lastNote :: LastNote, notePlace :: NotePlace}
    deriving (Eq,Show)