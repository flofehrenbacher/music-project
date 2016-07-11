{-# LANGUAGE FlexibleContexts #-}
module EasyNotes where

import DrawText
import DrawKeyboard
import IORefs
import UserInput
import MidiFun
import NoteLine
import Time
import Types

import Data.IORef
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT

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

idle ::  MyTime        -> (InputDeviceID,OutputDeviceID) -> DisplayInfo                                      ->  IdleCallback
idle     startTimeRef     (inputID, outputID)              (greenRef,redRef,songInfoRef,lastPlayedNote,xRef) = do
    difference <- computePassedTime startTimeRef
    xRef $= placeNoteToBePlayed difference
    currentMsg <- readMidi [inputID] [outputID]
    currentPitchClassPlayed <- midiToPitchClass currentMsg
    updateLastPlayedNote lastPlayedNote currentPitchClassPlayed
    songInfo <- readIORef songInfoRef
    updateSongInfo currentPitchClassPlayed songInfo greenRef redRef songInfoRef startTimeRef
    postRedisplay Nothing

display :: DisplayInfo -> DisplayCallback
display    displayInfo = do
    clear [ColorBuffer]
    loadIdentity
    preservingMatrix $ drawKeyboard displayInfo
    swapBuffers
    flush