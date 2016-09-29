-- | This module defines the idlecallback of /Easy Notes/
--
-- Here all background computations are made.
module Idle where

import DisplayInfo
import MidiFun
import Time
import View.Text

import Data.IORef
import Data.Time.Clock
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT

-- | Computes the time that has passed since the last right note was played.
--
-- Also receives MIDI events and passes it on if it is corresponding to a pressed key.
idle ::  IORef UTCTime -> (InputDeviceID,OutputDeviceID) -> IORef DisplayInfo -> IdleCallback
idle     startTimeRef     (inputID, outputID)              displayInfoRef = do
    difference <- computePassedTime startTimeRef
    displayInfo <- readIORef displayInfoRef 
    currentMsg <- readMidi [inputID] [outputID]
    midiDisplayInfo <- updateIsMidiKeyPressed currentMsg displayInfo
    currentPitchClassPlayed <- midiToPitchClass currentMsg
    newDisplayInfo <- updateDisplayInfo (midiDisplayInfo {notePlace = placeNoteToBePlayed difference}) startTimeRef currentPitchClassPlayed
    displayInfoRef $= newDisplayInfo
    postRedisplay Nothing