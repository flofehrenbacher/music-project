module MidiFun where

import DisplayInfo

import Euterpea.IO.MIDI.MidiIO
import Euterpea

-- | returns the first found midi input device
getFirstInputID :: IO InputDeviceID
getFirstInputID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

-- | returns the first found midi output device
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

updateIsMidiKeyPressed :: (Maybe (Time,[Message])) -> DisplayInfo -> IO DisplayInfo
updateIsMidiKeyPressed currentMsg displayInfo = do
    case currentMsg of
        Just (_,((NoteOn _ _ _) : _)) -> do
            return $ displayInfo {isMidiKeyPressed = True}
        Just (_,((ControlChange _ _ _) : _)) -> do
            return $ displayInfo {isMidiKeyPressed = False}
        Nothing -> return displayInfo

-- | checks if midi message corresponds to a note
-- and returns its pitch if it is a note
-- otherwise returns Nothing
filterNoteOn :: Message -> Maybe Pitch
filterNoteOn    (NoteOn _ key _) = Just $ pitch key
filterNoteOn    _                = Nothing


readMidi :: [InputDeviceID] -> [OutputDeviceID] -> IO (Maybe (Time,[Message]))
readMidi devsIn devsOut = do
    let transform [] = Nothing
        transform xs = Just $ map (\message -> (0, Std $ message)) xs
        signalToMessage Nothing = []
        signalToMessage (Just (t,message)) = message
    msgs <- sequence $ map pollMidi devsIn -- get MIDI messages coming
    let actual = head msgs
    let outputValue = transform $ concatMap signalToMessage msgs
    sequence $ map (\outputDevice -> sendMidiOut outputDevice outputValue) devsOut
    return actual

-- | sends a midi event to the specified outputDevice
sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
sendMidiOut dev message = outputMidi dev >>
    maybe (return ()) (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) message
