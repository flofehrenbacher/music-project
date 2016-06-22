{-# LANGUAGE FlexibleContexts #-}
module MidiMusicOpenGL where

import KeyboardEvents
import MidiFun
import StateRecorder

import Control.Concurrent.Chan
import Data.IORef
import Euterpea
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

new = newIORef

main :: IO()
main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 1024 900
    myChannel <- initMidi
    createAWindow progName myChannel
    mainLoop

createAWindow :: String -> Chan (Maybe (Time, [Message])) -> IO()
createAWindow windowName myChannel = do
    createWindow windowName
    recorder <- new $ MyRecorder []
    displayCallback $= display myChannel
    keyboardMouseCallback $= Just (keyboard recorder)

display :: Chan (Maybe (Time,[Message])) -> IO ()
display myChannel = do
    clear [ColorBuffer]
    loadIdentity
    swapBuffers
    flush