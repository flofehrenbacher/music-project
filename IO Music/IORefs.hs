module IORefs where

import DrawText
import SongCollection

import Data.IORef
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT

type GreenPlace = IORef (Maybe GLfloat)
type RedPlace = IORef (Maybe GLfloat)
type LastNote = IORef (Maybe PitchClass)
type SongInfo = IORef (Maybe (PitchClass, [Music Pitch]))
type NotePlace = IORef GLfloat

setUpIORefs :: IO ((GreenPlace, RedPlace, SongInfo, LastNote, NotePlace), IORef UTCTime)
setUpIORefs = do
    greenRef <- newIORef Nothing
    redRef <- newIORef Nothing
    songInfo <- getRestInfo song
    songInfoRef <- newIORef songInfo
    lastPlayedNote <- newIORef Nothing
    curTime <- getCurrentTime
    startTimeRef <- newIORef curTime
    xRef <- newIORef 0
    return ((greenRef,redRef,songInfoRef,lastPlayedNote,xRef),startTimeRef)