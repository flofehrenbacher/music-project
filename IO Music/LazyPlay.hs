module LazyPlay where

import MidiFun

import Control.Concurrent
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea
import HaskellOx4
-- wie kann ich spezielle Funktionen importieren

playNotesOnKeyboard :: IO()
playNotesOnKeyboard = do
    initializeMidi
    inputID <- getFirstDeviceID
    outputID <- getFirstOutputID
    loopy inputID outputID 

-- idleCallback
loopy :: InputDeviceID -> OutputDeviceID  -> IO()
loopy inputID outputID = readMidi ([inputID],[outputID], False) >>  loopy inputID outputID

initKeyboard :: IO (InputDeviceID, OutputDeviceID)
initKeyboard = do
    initializeMidi
    inputID <- getFirstDeviceID
    outputID <- getFirstOutputID
    return (inputID, outputID)

loopGL :: InputDeviceID -> OutputDeviceID  -> IO()
loopGL  inputID outputID = readMidi ([inputID],[outputID], False)