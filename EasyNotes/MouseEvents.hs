module MouseEvents where

import MidiFun
import DisplayInfo

import Data.IORef
import Euterpea
import Graphics.UI.GLUT

mouse :: IORef DisplayInfo -> OutputDeviceID -> MouseCallback
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 157 && realToFrac x < 193 && realToFrac y > 227 && realToFrac y < 350 = do
    sendPitchToSpeaker Cs displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 227 && realToFrac x < 263 && realToFrac y > 227 && realToFrac y < 350 = do
    sendPitchToSpeaker Ds displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 367 && realToFrac x < 403 && realToFrac y > 227 && realToFrac y < 350 = do
    sendPitchToSpeaker Fs displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 437 && realToFrac x < 473 && realToFrac y > 227 && realToFrac y < 350 = do
    sendPitchToSpeaker Gs displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 507 && realToFrac x < 543 && realToFrac y > 227 && realToFrac y < 350 = do
    sendPitchToSpeaker As displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 105 && realToFrac x < 175 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker C displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 175 && realToFrac x < 245 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker D displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 245 && realToFrac x < 315 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker E displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 315 && realToFrac x < 385 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker F displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 385 && realToFrac x < 455 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker G displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 455 && realToFrac x < 525 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker A displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 525 && realToFrac x < 595 && realToFrac y > 225 && realToFrac y < 425 = do
    sendPitchToSpeaker B displayInfoRef outputID
    keyPressed displayInfoRef True
mouse displayInfoRef outputID LeftButton Up (Position x y) = do
    stopSpeakers outputID
    keyPressed displayInfoRef False
mouse _ _ _ _ _ = return ()

keyPressed :: IORef DisplayInfo -> Bool    -> IO ()
keyPressed    displayInfoRef       press   = do
    displayInfo <- readIORef displayInfoRef
    let newDisplayInfo = displayInfo {isKeyPressed = press}
    displayInfoRef $= newDisplayInfo

sendPitchToSpeaker :: PitchClass -> IORef DisplayInfo -> OutputDeviceID -> IO ()
sendPitchToSpeaker    pc            displayInfoRef       outputID       = do
    displayInfo <- readIORef displayInfoRef
    let newDisplayInfo = displayInfo {lastNote = Just pc}
    sendMidiToSpeakers pc outputID
    displayInfoRef $= newDisplayInfo

sendMidiToSpeakers :: PitchClass -> OutputDeviceID -> IO()
sendMidiToSpeakers pc outputID = do
        let f [] = Nothing
            f xs = Just $ map (\m -> (0, Std $ m)) xs
            g Nothing = []
            g (Just (t,ms)) = ms
            outVal = f $ g (Just (0, [NoteOn {channel = 0, key = (absPitch (pc,4) `mod` 12) + 60, velocity = 127}]))
        sendMidiOut outputID outVal

stopSpeakers :: OutputDeviceID -> IO()
stopSpeakers outputID = do
        let f [] = Nothing
            f xs = Just $ map (\m -> (0, Std $ m)) xs
            g Nothing = []
            g (Just (t,ms)) = ms
            outVal = f $ g (Just (0, [ControlChange {channel = 0, controllerNumber = 123, controllerValue = 0}]))
        sendMidiOut outputID outVal