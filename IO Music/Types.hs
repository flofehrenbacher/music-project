module Types where

import Euterpea
import Data.IORef
import Graphics.UI.GLUT
import Data.Time.Clock

type GreenPlace = IORef (Maybe GLfloat)
type RedPlace = IORef (Maybe GLfloat)
type LastNote = IORef (Maybe PitchClass)
type SongInfo = IORef (Maybe (PitchClass, [Music Pitch]))
type NotePlace = IORef GLfloat
type MyTime = IORef UTCTime
type DisplayInfo = (GreenPlace, RedPlace, SongInfo, LastNote, NotePlace)
-- data DisplayInfo = Display {greenPlace :: GreenPlace}(GreenPlace, RedPlace, SongInfo, LastNote, NotePlace)