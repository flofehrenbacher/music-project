{-# LANGUAGE FlexibleContexts #-}
module MusicWithKeys where

import Euterpea

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import KeyboardEvents
import StateRecorder
import MidiPoll

--MIDI
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO

import Control.Concurrent.Chan
import Data.IORef
new = newIORef

main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 500 500
    channel <- initMidi
    createAWindow progName channel
    mainLoop

createAWindow windowName channel = do
    createWindow windowName
    recorder <- new $ MyRecorder []
    music <- new c
    displayCallback $= display channel
    keyboardMouseCallback $= Just (keyboard recorder)

display channel = do
    clear [ColorBuffer]
    loadIdentity
    message <- readChan channel
    print message
    swapBuffers
    flush