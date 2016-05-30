module MidiPoll where

import Control.Concurrent
import Control.Concurrent.Chan

--MIDI
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO

initMidi :: IO (Chan (Maybe (Time,[Message])))
initMidi = do
    (((deviceID,_):_) ,_) <- getAllDevices
    channel <- newChan
    _ <- forkIO $ midiLoop deviceID channel
    return channel
    
midiLoop :: InputDeviceID -> Chan (Maybe (Time,[Message])) ->IO ()
midiLoop deviceID channel = do
    midiMessage <- pollMidi deviceID
    writeChan channel midiMessage
    threadDelay 100000
    midiLoop deviceID channel