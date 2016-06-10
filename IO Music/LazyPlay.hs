module LazyPlay where

import MidiFun

import Control.Concurrent
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea

main :: IO()
main = getFirstDeviceID >>= readNotes >>= play

readNotes :: InputDeviceID -> IO (Music Pitch)
readNotes    devID         = do
    note <- readNote devID
    notes <- readNotes devID
    return $ note :+: notes

-- CASES IN MAYBE MIT DONOTATION
readNote :: InputDeviceID -> IO (Music Pitch)
readNote    devID         = do
    midiMessage <- pollMidi devID
    case midiMessage of
        Just (_,(message : _)) -> do
            case midiMessageToMusicPitch message of
                Nothing -> do
                    print "invalid Message"
                    return (rest wn)
                Just musicPitch -> do
                    print "test"
                    return musicPitch
        Nothing -> print "no message" >> return (rest wn)