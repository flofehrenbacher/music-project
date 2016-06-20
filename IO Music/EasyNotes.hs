{-# LANGUAGE FlexibleContexts #-}
module EasyNotes where

import DrawText
import DrawFun
import DrawKeyboard
import LazyPlay
import SongCollection

import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

showKeyboard :: IO ()
showKeyboard = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 1366 768
    createWindow progName
    (inputID, outputID) <- initKeyboard
    xRef <- newIORef 0
    startTime <- getCurrentTime
    idleCallback $= Just (idle xRef startTime (inputID, outputID))
    displayCallback $= display xRef
    mainLoop

idle :: IORef GLfloat -> UTCTime -> (InputDeviceID,OutputDeviceID) -> IdleCallback
idle xRef startTime (inputID, outputID) = do
    time <- getCurrentTime
    xRef $= sin ( realToFrac ( diffUTCTime time startTime ) ) / 1.65
    loopGL inputID outputID
    postRedisplay Nothing

display xRef = do
    clear [ColorBuffer]
    loadIdentity
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    currentColor $= Color4 1 1 1 1
    preservingMatrix $ keyboardFigure
    x <- readIORef xRef
    showNotesOfList haenschenList x
    swapBuffers
    flush