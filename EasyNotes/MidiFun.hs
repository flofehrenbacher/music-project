module MidiFun where

import Euterpea.IO.MIDI.MidiIO
import Euterpea

-- | returns the first midi input device
getFirstInputID :: IO InputDeviceID
getFirstInputID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

-- | returns the last midi output device
getFirstOutputID :: IO OutputDeviceID
getFirstOutputID = do
    (_ ,((deviceID,_):_)) <- getAllDevices
    return deviceID

-- | returns a tuple of first midi input device and first midi output device
initDevices :: IO (InputDeviceID, OutputDeviceID)
initDevices = do
    initializeMidi
    inputID <- getFirstInputID
    outputID <- getFirstOutputID
    return (inputID, outputID)

-- | transforms a midi message into a Music Pitch
-- if the message corresponds to a note
-- otherwise returns Nothing
midiMessageToMusicPitch :: Message -> Maybe (Music Pitch)
midiMessageToMusicPitch    message = (Prim . Note wn) <$> filterNoteOn message

-- | transforms a midi event into a PitchClass
-- if there actually is a midi event and it could be transformed to a music pitch
-- otherwise returns Nothing
midiToPitchClass :: (Maybe (Time,[Message])) -> IO (Maybe PitchClass)
midiToPitchClass (Just (_,(message : _))) = do
    case midiMessageToMusicPitch message of
        Just musicPitch -> do
            return (fmap fst (filterNoteOn message))
        Nothing         -> return Nothing
midiToPitchClass _ = return Nothing

-- | checks if midi message corresponds to a note
-- and returns its pitch if it is a note
-- otherwise returns Nothing
filterNoteOn :: Message -> Maybe Pitch
filterNoteOn    (NoteOn _ key _) = Just $ pitch key
filterNoteOn    _                = Nothing

-- | sends a midi event to the specified outputDevice
sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
sendMidiOut dev ms = outputMidi dev >> 
    maybe (return ()) (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms
