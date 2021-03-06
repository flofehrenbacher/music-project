-- | This module contains everything concerning the MIDI devices / events
module MidiFun where

import DisplayInfo

import Euterpea.IO.MIDI.MidiIO
import Euterpea

-- | Returns the first found midi input device
getFirstInputID :: IO InputDeviceID
getFirstInputID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

-- | Returns the first found midi output device
getFirstOutputID :: IO OutputDeviceID
getFirstOutputID = do
    (_ ,((deviceID,_):_)) <- getAllDevices
    return deviceID

-- | Returns a tuple of first midi input device and first midi output device
initDevices :: IO (InputDeviceID, OutputDeviceID)
initDevices = do
    initializeMidi
    inputID <- getFirstInputID
    outputID <- getFirstOutputID
    return (inputID, outputID)

-- | Transforms a midi message into a Music Pitch.
-- if the 'Message' corresponds to a note.
-- Otherwise returns 'Nothing'
midiMessageToMusicPitch :: Message -> Maybe (Music Pitch)
midiMessageToMusicPitch    message = (Prim . Note wn) <$> filterNoteOn message

-- | Transforms a midi event into a PitchClass
-- if there actually is a midi event and it could be transformed to a music pitch
-- Otherwise returns 'Nothing'
midiToPitchClass :: (Maybe (Time,[Message])) -> IO (Maybe PitchClass)
midiToPitchClass (Just (_,(message : _))) = do
    case midiMessageToMusicPitch message of
        Just musicPitch -> do
            return (fmap fst (filterNoteOn message))
        Nothing         -> return Nothing
midiToPitchClass _ = return Nothing

-- | Updates in the 'DisplayInfo' weather there is currently a key pressed on the midi device
updateIsMidiKeyPressed :: (Maybe (Time,[Message])) -> DisplayInfo -> IO DisplayInfo
updateIsMidiKeyPressed currentMsg displayInfo = do
    case currentMsg of
        Just (_,((NoteOn _ _ _) : _)) -> do
            return $ displayInfo {isMidiKeyPressed = True}
        Just (_,((ControlChange _ _ _) : _)) -> do
            return $ displayInfo {isMidiKeyPressed = False}
        _ -> return displayInfo

-- | Checks whether a midi 'Message' corresponds to a note
-- and returns its pitch if it is a note.
-- Otherwise returns 'Nothing'
filterNoteOn :: Message -> Maybe Pitch
filterNoteOn    (NoteOn _ key _) = Just $ pitch key
filterNoteOn    _                = Nothing

-- | Reads all MIDI events received from the input devices and sends them to the output devices
-- 
-- Returns the latest MIDI event.
readMidi :: [InputDeviceID] -> [OutputDeviceID] -> IO (Maybe (Time,[Message]))
readMidi devsIn devsOut = do
    let transform [] = Nothing
        transform xs = Just $ map (\message -> (0, Std $ message)) xs
        signalToMessage Nothing = []
        signalToMessage (Just (t,message)) = message
    events <- sequence $ map pollMidi devsIn -- get MIDI messages coming
    let latestMidiEvent = head events
    let outputValue = transform $ concatMap signalToMessage events
    sequence $ map (\outputDevice -> sendMidiOut outputDevice outputValue) devsOut
    return latestMidiEvent

-- | Sends a midi event to the specified outputDevice
sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
sendMidiOut dev message = outputMidi dev >>
    maybe (return ()) (mapM_ (\(t,message) -> deliverMidiEvent dev (0, message))) message
