module Idle where

import DisplayInfo
import MidiFun
import Graphics.UI.GLUT
import Time
import Euterpea.IO.MIDI.MidiIO
import Data.IORef
import View.Text

idle ::  MyTime        -> (InputDeviceID,OutputDeviceID) -> IORef DisplayInfo -> IdleCallback
idle     startTimeRef     (inputID, outputID)              displayInfoRef = do
    difference <- computePassedTime startTimeRef
    displayInfo <- readIORef displayInfoRef 
    newDisplayInfo <- updateSongInfo (displayInfo {notePlace = placeNoteToBePlayed difference}) startTimeRef
    displayInfoRef $= newDisplayInfo
    postRedisplay Nothing