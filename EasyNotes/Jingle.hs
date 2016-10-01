-- | This module defines a short /jingle/ which is played at the beginning of /Easy Notes/.
--
-- This allows the program to initialize completely!
module Jingle where

import MouseEvents
import Euterpea

import Control.Concurrent

-- | sends a short jingle to the output device with the specified 'OutputDeviceID'
jingle :: OutputDeviceID -> IO ()
jingle outputID = do
    sendPitchClassToSpeakers C outputID
    threadDelay 500000
    sendPitchClassToSpeakers E outputID
    threadDelay 500000
    sendPitchClassToSpeakers G outputID
    threadDelay 500000
    sendPitchClassToSpeakers E outputID
    threadDelay 250000
    sendPitchClassToSpeakers D outputID
    threadDelay 250000
    sendPitchClassToSpeakers C outputID
    threadDelay 500000
    stopSpeakers outputID