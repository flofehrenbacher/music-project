{-# LANGUAGE FlexibleContexts #-}
module EasyNotes where

import View.Text
import View.Keyboard
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
    initialWindowSize $= Size 700 500
    createWindow progName
    (displayInfo, startTimeRef) <- setUpIORefs
    (inputID, outputID) <- initKeyboard
    idleCallback    $= Just    (idle startTimeRef (inputID, outputID) displayInfo)
    displayCallback $= display displayInfo
    reshapeCallback $= Just reshape
    mainLoop

idle ::  MyTime        -> (InputDeviceID,OutputDeviceID) -> DisplayInfo                                      ->  IdleCallback
idle     startTimeRef     (inputID, outputID)              (greenRef,redRef,songInfoRef,lastPlayedNote,placeForNote) = do
    difference <- computePassedTime startTimeRef
    placeForNote $= placeNoteToBePlayed difference
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
    
reshape :: ReshapeCallback
reshape (Size width height) = do
    viewport $= (Position ((width - 700) `div` 2)  ((height - 500) `div` 2), (Size 700 500))
    postRedisplay Nothing