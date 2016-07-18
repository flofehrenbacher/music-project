module MidiFun where

import Control.Concurrent

import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea

getFirstDeviceID :: IO InputDeviceID
getFirstDeviceID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

getFirstOutputID :: IO OutputDeviceID
getFirstOutputID = do
    (_ ,((deviceID,_):_)) <- getAllDevices
    return deviceID

initDevices :: IO (InputDeviceID, OutputDeviceID)
initDevices = do
    initializeMidi
    inputID <- getFirstDeviceID
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

readMidi :: [InputDeviceID] -> [OutputDeviceID] -> IO (Maybe (Time,[Message]))
readMidi devsIn devsOut = do
    let f [] = Nothing
        f xs = Just $ map (\m -> (0, Std $ m)) xs
        g Nothing = []
        g (Just (t,ms)) = ms
    msgs <- sequence $ map pollMidi devsIn -- get MIDI messages coming
    let actual = head msgs 
    let outVal = f $ concatMap g msgs
    sequence $ map (\d -> sendMidiOut d outVal) devsOut
    return actual

sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
sendMidiOut dev ms = outputMidi dev >> 
    maybe (return ()) (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms
