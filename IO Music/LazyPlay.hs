module LazyPlay where

import Control.Concurrent

--MIDI
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea

import MidiPoll

main :: IO()
main = getFirstDeviceID >>= readNotes >> main

readNotes :: InputDeviceID -> IO (Music Pitch)
readNotes    devID         = do
    note <- readNote devID
    case note of
        Prim (Rest _) -> play (rest wn :+: rest wn :+: c 4 en)
        aNote         -> play aNote
    readNotes devID

readNote :: InputDeviceID -> IO (Music Pitch)
readNote    devID         = do
    initializeMidi
    midiMessage <- pollMidi devID
    case midiMessage of
        Just (_,(message : _)) -> do
            case midiMessageToMusicPitch message of
                Nothing -> return (rest 0)
                Just musicPitch -> return (musicPitch)
        Nothing -> return (rest 0)