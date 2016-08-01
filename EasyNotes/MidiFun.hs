module MidiFun where

import Euterpea.IO.MIDI.MidiIO
import Euterpea

getFirstInputID :: IO InputDeviceID
getFirstInputID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

getFirstOutputID :: IO OutputDeviceID
getFirstOutputID = do
    (_ ,((deviceID,_):_)) <- getAllDevices
    return deviceID

initDevices :: IO (InputDeviceID, OutputDeviceID)
initDevices = do
    initializeMidi
    inputID <- getFirstInputID
    outputID <- getFirstOutputID
    return (inputID, outputID)

midiMessageToMusicPitch :: Message -> Maybe (Music Pitch)
midiMessageToMusicPitch    message = (Prim . Note wn) <$> filterNoteOn message

midiToPitchClass :: (Maybe (Time,[Message])) -> IO (Maybe PitchClass)
midiToPitchClass (Just (_,(message : _))) = do
    case midiMessageToMusicPitch message of
        Just musicPitch -> do
            return (fmap fst (filterNoteOn message))
        Nothing         -> return Nothing
midiToPitchClass _ = return Nothing

filterNoteOn :: Message -> Maybe Pitch
filterNoteOn    (NoteOn _ key _) = Just $ pitch key
filterNoteOn    _                = Nothing

-- modified version of donya quicks readMidi
sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
sendMidiOut dev ms = outputMidi dev >> 
    maybe (return ()) (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms
