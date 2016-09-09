module MouseEvents where

import MidiFun
import DisplayInfo

import Data.IORef
import Euterpea
import Graphics.UI.GLUT

xStartWhiteKey = 105
yStartWhiteKey = 225
yEndWhiteKey   = 425

xStartBlackKey = 157
yStartBlackKey = 227
yEndBlackKey   = 350

mouse :: IORef DisplayInfo -> OutputDeviceID -> MouseCallback
mouse    displayInfoRef       outputID          LeftButton  Down  (Position x y) = do
    (Size xSize ySize) <- get windowSize
    playAccordingToPosition    displayInfoRef    outputID    (Position x y) xSize ySize
mouse displayInfoRef outputID LeftButton Up _ = do
    stopSpeakers outputID
    keyPressed displayInfoRef False
mouse _              _        _          _  _ = return ()

playAccordingToPosition :: IORef DisplayInfo -> OutputDeviceID -> Position -> GLint -> GLint ->  IO ()
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Cs x xSize && isYPositionBlackKey y ySize = do
    sendPitchToSpeaker Cs displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Ds x xSize && isYPositionBlackKey y ySize = do
    sendPitchToSpeaker Ds displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Fs x xSize && isYPositionBlackKey y ySize = do
    sendPitchToSpeaker Fs displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Gs x xSize && isYPositionBlackKey y ySize = do
    sendPitchToSpeaker Gs displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey As x xSize && isYPositionBlackKey y ySize = do
    sendPitchToSpeaker As displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey C x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker C displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey D x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker D displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey E x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker E displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey F x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker F displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey G x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker G displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey A x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker A displayInfoRef outputID
playAccordingToPosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey B x xSize && isYPositionWhiteKey y ySize = do
    sendPitchToSpeaker B displayInfoRef outputID
playAccordingToPosition _ _ _ _ _= return ()


isXPositionBlackKey :: PitchClass -> GLint -> GLint -> Bool
isXPositionBlackKey    pitchClass x xSize = keyStartPosition < fromEnum x && keyEndPosition > fromEnum x
                                            where keyStartPosition | pitchClass < E = fromEnum ((xSize - 700) `div` 2) + xStartBlackKey + (((absPitch (pitchClass,4)) `mod` 61) `div` 2) * 70
                                                                   | otherwise      = fromEnum ((xSize - 700) `div` 2) + xStartBlackKey + 3 * 70 + (((absPitch (pitchClass,4)) `mod` 66) `div` 2) * 70
                                                  keyEndPosition   = keyStartPosition + 36

isYPositionBlackKey :: GLint -> GLint -> Bool
isYPositionBlackKey    y        ySize = fromEnum y > (yStartBlackKey + (fromEnum ((ySize - 500) `div` 2))) && fromEnum y < (yEndBlackKey + (fromEnum ((ySize - 500) `div` 2)))

isXPositionWhiteKey :: PitchClass -> GLint -> GLint -> Bool
isXPositionWhiteKey    pitchClass    x        xSize = keyStartPosition < fromEnum x && keyEndPosition > fromEnum x
                                                        where keyStartPosition | pitchClass < F = fromEnum ((xSize - 700) `div` 2) + xStartWhiteKey + (((absPitch (pitchClass,4)) `mod` 60) `div` 2) * 70
                                                                               | otherwise      = fromEnum ((xSize - 700) `div` 2) + xStartWhiteKey + 3 * 70 + (((absPitch (pitchClass,4)) `mod` 65) `div` 2) * 70
                                                              keyEndPosition   = keyStartPosition + 70

isYPositionWhiteKey :: GLint -> GLint -> Bool
isYPositionWhiteKey    y        ySize = fromEnum y > (yStartWhiteKey + (fromEnum ((ySize - 500) `div` 2))) && fromEnum y < (yEndWhiteKey + (fromEnum ((ySize - 500) `div` 2)))

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
    keyPressed displayInfoRef True

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