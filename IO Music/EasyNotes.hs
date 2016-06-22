{-# LANGUAGE FlexibleContexts #-}
module EasyNotes where

import DrawText
import DrawFun
import DrawKeyboard
import IORefs
import LazyPlay
import MidiFun
import SongCollection
import Time

import Data.IORef
import Data.Time.Clock
import Euterpea
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

type DisplayInfo = (GreenPlace, RedPlace, SongInfo, LastNote, NotePlace)

main :: IO ()
main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 1366 768
    createWindow progName
    (displayInfo, startTimeRef) <- setUpIORefs
    (inputID, outputID) <- initKeyboard
    idleCallback    $= Just    (idle startTimeRef (inputID, outputID) displayInfo)
    displayCallback $= display displayInfo
    mainLoop

idle ::  IORef UTCTime -> (InputDeviceID,OutputDeviceID) -> DisplayInfo                                      ->  IdleCallback
idle     startTimeRef     (inputID, outputID)              (greenRef,redRef,songInfoRef,lastPlayedNote,xRef) = do
    difference <- computePassedTime startTimeRef
    xRef $= placeNoteToBePlayed difference
    currentMsg <- readMidi [inputID] [outputID]
    currentPitchClassPlayed <- midiToPitchClass currentMsg
    case currentPitchClassPlayed of
        Just pc -> do
            lastPlayedNote $= currentPitchClassPlayed
        Nothing -> return ()
    songInfo <- readIORef songInfoRef
    case songInfo of
        Nothing -> return ()
        Just (noteToBePlayed, restNotes) -> do
            if isAbsPitchTheSame (Just noteToBePlayed) currentPitchClassPlayed then do
                greenRef $= findPlaceFor noteToBePlayed
                redRef $= Nothing
                songInfo <- getRestInfo restNotes
                songInfoRef $= songInfo
                curTime <- getCurrentTime
                startTimeRef $= curTime 
            else do
                case currentPitchClassPlayed of
                    Nothing -> return ()
                    Just pc -> do 
                        redRef $= findPlaceFor pc
                        greenRef $= Nothing
    postRedisplay Nothing

display :: DisplayInfo                                      -> DisplayCallback
display   (greenRef,redRef,songInfoRef,lastPlayedNote,xRef) = do
    clear [ColorBuffer]
    loadIdentity
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    x <- readIORef xRef
    currentPitchClassPlayed <- readIORef lastPlayedNote
    songInfo <- readIORef songInfoRef
    case songInfo of
        -- song ends
        Nothing -> do
            showTextAboveKeyboard "Song finished!" (-0.85)
            preservingMatrix $ keyboardFigure Nothing Nothing
        -- song continues
        Just (noteToBePlayed, restNotes) -> do
            showTextAboveKeyboard (pitchToString noteToBePlayed) x
            xRightNote <- readIORef greenRef
            xFalseNote <- readIORef redRef
            preservingMatrix $ keyboardFigure xRightNote xFalseNote
    swapBuffers
    flush