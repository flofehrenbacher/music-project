module MidiFun where

import Control.Concurrent

import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea

initMidi :: IO (Chan (Maybe (Time,[Message])))
initMidi = do
    deviceID <- getFirstDeviceID
    channel <- newChan
    _ <- forkIO $ midiLoop deviceID channel
    return channel

getFirstDeviceID :: IO InputDeviceID
getFirstDeviceID = do
    (((deviceID,_):_) ,_) <- getAllDevices
    return deviceID

getFirstOutputID :: IO OutputDeviceID
getFirstOutputID = do
    (_ ,((deviceID,_):_)) <- getAllDevices
    return deviceID

initKeyboard :: IO (InputDeviceID, OutputDeviceID)
initKeyboard = do
    initializeMidi
    inputID <- getFirstDeviceID
    outputID <- getFirstOutputID
    return (inputID, outputID)
    
midiLoop :: InputDeviceID -> Chan (Maybe (Time,[Message])) -> IO ()
midiLoop deviceID channel = do
    initializeMidi
    midiMessage <- pollMidi deviceID
    case midiMessage of
        Nothing -> return ()
        Just x -> writeChan channel midiMessage
    threadDelay 100000
    midiLoop deviceID channel

midiMessageToMusicPitch :: Message -> Maybe (Music Pitch)
midiMessageToMusicPitch    message = (Prim . Note wn) <$> filterNoteOn message

midiMessageToPlay :: (Maybe (Time,[Message])) -> IO (Maybe PitchClass)
midiMessageToPlay (Just (_,(message : _))) = do
    case midiMessageToMusicPitch message of
        Just musicPitch -> do 
            _ <- forkIO $ play musicPitch
            return (fmap fst (filterNoteOn message))
        Nothing         -> return Nothing
midiMessageToPlay _ = return Nothing

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