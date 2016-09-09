module Jingle where

import MouseEvents
import Euterpea

import Control.Concurrent

-- | plays a short jingle
jingle :: OutputDeviceID -> IO ()
jingle outputID = do
    sendMidiToSpeakers C outputID
    threadDelay 500000
    sendMidiToSpeakers E outputID
    threadDelay 500000
    sendMidiToSpeakers G outputID
    threadDelay 500000
    sendMidiToSpeakers E outputID
    threadDelay 250000
    sendMidiToSpeakers D outputID
    threadDelay 250000
    sendMidiToSpeakers C outputID
    threadDelay 500000
    stopSpeakers outputID