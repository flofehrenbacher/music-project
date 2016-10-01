-- | This module defines what happens when the mousecallback is called
module MouseEvents where

import MidiFun
import DisplayInfo
import WindowSize

import Data.IORef
import Euterpea
import Graphics.UI.GLUT

-- | Points with which the position of white keys can be determined.
xStartWhiteKey, yStartWhiteKey, yEndWhiteKey :: Int
xStartWhiteKey = 105
yStartWhiteKey = 225
yEndWhiteKey   = 425

-- | Points with which the position of black keys can be determined.
xStartBlackKey, yStartBlackKey, yEndBlackKey :: Int
xStartBlackKey = 157
yStartBlackKey = 227
yEndBlackKey   = 350

-- | If the left key of the mouse is pressed reacts according to the position.
--
-- When the left key is released again stops the speakers and updates in the 'DisplayInfo' that 'isScreenKeyPressed' is False.
mouse :: IORef DisplayInfo -> OutputDeviceID -> MouseCallback
mouse    displayInfoRef       outputID          LeftButton  Down  (Position x y) = do
    (Size xSize ySize) <- get windowSize
    reactToMousePosition    displayInfoRef    outputID    (Position x y) xSize ySize
mouse displayInfoRef outputID LeftButton Up _ = do
    stopSpeakers outputID
    keyPressed displayInfoRef False
mouse _              _        _          _  _ = return ()

-- | According to the current mouse position reacts appropriately
reactToMousePosition :: IORef DisplayInfo -> OutputDeviceID -> Position -> GLint -> GLint ->  IO ()
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Cs x xSize && isYPositionBlackKey y ySize = do
    reactToIncomingPitchClass Cs displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Ds x xSize && isYPositionBlackKey y ySize = do
    reactToIncomingPitchClass Ds displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Fs x xSize && isYPositionBlackKey y ySize = do
    reactToIncomingPitchClass Fs displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey Gs x xSize && isYPositionBlackKey y ySize = do
    reactToIncomingPitchClass Gs displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionBlackKey As x xSize && isYPositionBlackKey y ySize = do
    reactToIncomingPitchClass As displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey C x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass C displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey D x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass D displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey E x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass E displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey F x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass F displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey signalToMessage x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass signalToMessage displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey A x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass A displayInfoRef outputID
reactToMousePosition displayInfoRef outputID (Position x y) xSize ySize| isXPositionWhiteKey B x xSize && isYPositionWhiteKey y ySize = do
    reactToIncomingPitchClass B displayInfoRef outputID
reactToMousePosition _ _ _ _ _= return ()

-- | Checks if the x-Position of the mouse is referring to a specified pitch class that corresponds to a black key
isXPositionBlackKey :: PitchClass -> GLint -> GLint -> Bool
isXPositionBlackKey    pitchClass x xSize = keyStartPosition < fromEnum x && keyEndPosition > fromEnum x
                                            where keyStartPosition | pitchClass < E = fromEnum ((xSize - initWidth) `div` 2) + xStartBlackKey + (((absPitch (pitchClass,4)) `mod` 61) `div` 2) * 70
                                                                   | otherwise      = fromEnum ((xSize - initWidth) `div` 2) + xStartBlackKey + 3 * 70 + (((absPitch (pitchClass,4)) `mod` 66) `div` 2) * 70
                                                  keyEndPosition   = keyStartPosition + 36

-- | Checks if the y-Position of the mouse is referring to a black key
isYPositionBlackKey :: GLint -> GLint -> Bool
isYPositionBlackKey    y        ySize = fromEnum y > (yStartBlackKey + (fromEnum ((ySize - initHeight) `div` 2))) && fromEnum y < (yEndBlackKey + (fromEnum ((ySize - initHeight) `div` 2)))

-- | Checks if the x-Position of the mouse is referring to a specified pitch class that corresponds to a white key
isXPositionWhiteKey :: PitchClass -> GLint -> GLint -> Bool
isXPositionWhiteKey    pitchClass    x        xSize = keyStartPosition < fromEnum x && keyEndPosition > fromEnum x
                                                        where keyStartPosition | pitchClass < F = fromEnum ((xSize - initWidth) `div` 2) + xStartWhiteKey + (((absPitch (pitchClass,4)) `mod` 60) `div` 2) * 70
                                                                               | otherwise      = fromEnum ((xSize - initWidth) `div` 2) + xStartWhiteKey + 3 * 70 + (((absPitch (pitchClass,4)) `mod` 65) `div` 2) * 70
                                                              keyEndPosition   = keyStartPosition + 70

-- | Checks if the y-Position of the mouse is referring to a white key
isYPositionWhiteKey :: GLint -> GLint -> Bool
isYPositionWhiteKey    y        ySize = fromEnum y > (yStartWhiteKey + (fromEnum ((ySize - initHeight) `div` 2))) && fromEnum y < (yEndWhiteKey + (fromEnum ((ySize - initHeight) `div` 2)))

-- | Updates in the 'DisplayInfo' the last note and the current note,
--   sets the truth value of True that a key is currently pressed on the screen and then sends the pitch class to the speaker
reactToIncomingPitchClass :: PitchClass -> IORef DisplayInfo -> OutputDeviceID -> IO ()
reactToIncomingPitchClass    pc            displayInfoRef       outputID       = do
    displayInfo <- readIORef displayInfoRef
    let newDisplayInfo = displayInfo {lastNote = Just pc, currentNote = Just pc, isScreenKeyPressed = True}
    displayInfoRef $= newDisplayInfo
    sendPitchClassToSpeakers pc outputID

-- | Sends a midi event which corresponds to the specified PitchClass (/in octave 4/) to the specified outputDevice
sendPitchClassToSpeakers :: PitchClass -> OutputDeviceID -> IO()
sendPitchClassToSpeakers pc outputID = do
        let transform [] = Nothing
            transform xs = Just $ map (\message -> (0, Std $ message)) xs
            signalToMessage Nothing = []
            signalToMessage (Just (t,ms)) = ms
            outputValue = transform $ signalToMessage (Just (0, [NoteOn {channel = 0, key = (absPitch (pc,4) `mod` 12) + 60, velocity = 127}]))
        sendMidiOut outputID outputValue

-- | Makes the output device stop splaying the current pitch
stopSpeakers :: OutputDeviceID -> IO()
stopSpeakers outputID = do
        let transform [] = Nothing
            transform xs = Just $ map (\message -> (0, Std $ message)) xs
            signalToMessage Nothing = []
            signalToMessage (Just (t,ms)) = ms
            outputValue = transform $ signalToMessage (Just (0, [ControlChange {channel = 0, controllerNumber = 123, controllerValue = 0}]))
        sendMidiOut outputID outputValue