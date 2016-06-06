module MidiPoll where

import Control.Concurrent
import Control.Concurrent.Chan
import System.Mem

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import HaskellMusic

--MIDI
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
    
midiLoop :: InputDeviceID -> Chan (Maybe (Time,[Message])) -> IO ()
midiLoop deviceID channel = do
    initializeMidi
    midiMessage <- pollMidi deviceID
    case midiMessage of
        Nothing -> return ()
        Just x -> writeChan channel midiMessage
    threadDelay 100000
    midiLoop deviceID channel


filterNoteOn :: Message -> Maybe Pitch
filterNoteOn    (NoteOn _ key _) = Just $ pitch key
filterNoteOn    _                = Nothing

midiMessageToMusicPitch :: Message -> Maybe (Music Pitch)
midiMessageToMusicPitch    message = (Prim . Note 0.05) <$> filterNoteOn message

midiMessageToPlay :: (Maybe (Time,[Message])) -> IO (Maybe PitchClass)
midiMessageToPlay (Just (_,(message : _))) = do
    case midiMessageToMusicPitch message of
        Just musicPitch -> do 
            _ <- forkIO $ play musicPitch
            return (fmap fst (filterNoteOn message))
        Nothing         -> return Nothing
midiMessageToPlay _ = return Nothing

playDisplayedNotes :: [Music Pitch] -> IO ()
playDisplayedNotes    []            = return ()
playDisplayedNotes    ((Prim (Note _ ((pitchClass,_)))) : notes) = do
    renderString MonoRoman (pitchToString pitchClass)
    print (pitchToString pitchClass)
    playDisplayedNotes notes
playDisplayedNotes    _             = do
    renderString MonoRoman "ERROR"

pitchToString :: PitchClass -> String
pitchToString    C          = "C"
pitchToString    D          = "D"
pitchToString    E          = "E"
pitchToString    F          = "F"
pitchToString    G          = "G"
pitchToString    A          = "A"
pitchToString    B          = "B"
pitchToString    _          = "ich bin eine schwarze Taste"