module Idle where

import DisplayInfo
import MidiFun
import Time
import View.Text

import Data.IORef
import Data.Time.Clock
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT

idle ::  IORef UTCTime -> (InputDeviceID,OutputDeviceID) -> IORef DisplayInfo -> IdleCallback
idle     startTimeRef     (inputID, outputID)              displayInfoRef = do
    difference <- computePassedTime startTimeRef
    displayInfo <- readIORef displayInfoRef 
    newDisplayInfo <- updateSongInfo (displayInfo {notePlace = placeNoteToBePlayed difference}) startTimeRef
    displayInfoRef $= newDisplayInfo
    postRedisplay Nothing