module MouseEvents where

-- import IORefs
import MidiFun
import Types

import Data.IORef
import Euterpea
import Graphics.UI.GLUT

mouse :: IORef DisplayInfo -> OutputDeviceID-> MouseCallback
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 158 && realToFrac x < 190 && realToFrac y > 228 && realToFrac y < 348 = do
    sendPitchToSpeaker Cs displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 227 && realToFrac x < 262 && realToFrac y > 228 && realToFrac y < 348 = do
    sendPitchToSpeaker Ds displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 367 && realToFrac x < 400 && realToFrac y > 228 && realToFrac y < 348 = do
    sendPitchToSpeaker Fs displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 436 && realToFrac x < 472 && realToFrac y > 228 && realToFrac y < 348 = do
    sendPitchToSpeaker Gs displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 507 && realToFrac x < 541 && realToFrac y > 228 && realToFrac y < 348 = do
    sendPitchToSpeaker As displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 105 && realToFrac x < 170 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker C displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 175 && realToFrac x < 240 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker D displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 244 && realToFrac x < 310 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker E displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 315 && realToFrac x < 381 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker F displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 384 && realToFrac x < 451 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker G displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 455 && realToFrac x < 520 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker A displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Down (Position x y) | realToFrac x > 525 && realToFrac x < 590 && realToFrac y > 224 && realToFrac y < 424 = do
    sendPitchToSpeaker B displayInfoRef outputID
mouse displayInfoRef outputID LeftButton Up (Position x y) = do
    stopSpeakers outputID
mouse displayInfoRef _ _ _ _ = return ()

sendPitchToSpeaker :: PitchClass -> IORef DisplayInfo -> OutputDeviceID -> IO ()
sendPitchToSpeaker    pc displayInfoRef outputID = do
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
