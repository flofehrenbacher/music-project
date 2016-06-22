module LazyPlay where

import MidiFun

import Control.Concurrent
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea

playNotesOnKeyboard :: IO()
playNotesOnKeyboard = do
    initializeMidi
    inputID <- getFirstDeviceID
    outputID <- getFirstOutputID
    loopy inputID outputID 

-- idleCallback
loopy :: InputDeviceID -> OutputDeviceID  -> IO()
loopy inputID outputID = readMidi [inputID] [outputID] >>  loopy inputID outputID

initKeyboard :: IO (InputDeviceID, OutputDeviceID)
initKeyboard = do
    initializeMidi
    inputID <- getFirstDeviceID
    outputID <- getFirstOutputID
    return (inputID, outputID)

isAbsPitchTheSame :: Maybe PitchClass -> Maybe PitchClass -> Bool
isAbsPitchTheSame   (Just  pcOne)       (Just pcTwo)      | pcToInt pcOne == pcToInt pcTwo = True
                                                          | otherwise = False
isAbsPitchTheSame   _                   _                 = False