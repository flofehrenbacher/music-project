module IORefs where

import UserInput
import SongCollection
import DisplayInfo

import Data.IORef
import Data.Time.Clock

setUpIORefs :: Song -> IO (IORef DisplayInfo, IORef UTCTime)
setUpIORefs    song = do
    displayInfoRef <- newIORef $ DisplayInfo {greenPlace = Nothing, redPlace = Nothing, songInfo = getRestInfo song, lastNote = Nothing, notePlace = 1.4}
    curTime <- getCurrentTime
    startTimeRef <- newIORef curTime
    return (displayInfoRef,startTimeRef)
