module IORefs where

import UserInput
import SongCollection
import Types

import Data.IORef
import Data.Time.Clock

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