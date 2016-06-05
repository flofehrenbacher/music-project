{-# LANGUAGE FlexibleContexts #-}
module MusicWithKeys where

import Euterpea

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import Control.Concurrent

import KeyboardEvents
import StateRecorder
import MidiPoll
import HaskellMusic

--MIDI
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO

import Control.Concurrent.Chan
import Data.IORef
new = newIORef

main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 1024 900
    myChannel <- initMidi
    createAWindow progName myChannel
    mainLoop

createAWindow windowName myChannel = do
    createWindow windowName
    recorder <- new $ MyRecorder []
    displayCallback $= display myChannel
    keyboardMouseCallback $= Just (keyboard recorder)

display myChannel = do
    clear [ColorBuffer]
    loadIdentity
    forkIO $ doSomeThing myChannel
    swapBuffers
    flush
    
doSomeThing myChannel = do
    midiMessage <- readChan myChannel
    case midiMessage of
        Just something -> do 
            pitchClass <- transformMidiKeyToPitch $ midiMessage
            case pitchClass of
                Just x -> do
                    scale 0.0005 0.0005 (0.0005::GLfloat)
                    print x
                    renderString MonoRoman (pitchToString x)
                Nothing -> return ()
        Nothing -> return ()