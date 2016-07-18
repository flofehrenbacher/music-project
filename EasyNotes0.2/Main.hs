{-# LANGUAGE FlexibleContexts #-}
module Main where

import Euterpea
import IORefs
import UserInput
import MidiFun
import Modi
import MouseEvents
import Time
import Types
import View.Text
import View.Keyboard
import SongCollection

import Data.IORef
import Euterpea.IO.MIDI.MidiIO
import Graphics.UI.GLUT

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    arguments <- setModusAndSong args
    case arguments of
        (Just myModus, Just song) -> do
            initialDisplayMode $= [DoubleBuffered]
            initialWindowSize $= Size 700 500
            createWindow progName
            (displayInfoRef, startTimeRef) <- setUpIORefs song
            (inputID, outputID) <- initKeyboard
            mouseCallback $= Just (mouse displayInfoRef outputID)
            idleCallback    $= Just    (idle startTimeRef (inputID, outputID) displayInfoRef)
            displayCallback $= display displayInfoRef myModus
            reshapeCallback $= Just reshape
            mainLoop
        _ -> do
            putStrLn "The first argument must be: easy, medium or hard"
            putStrLn "The second one the song that is being played"

idle ::  MyTime        -> (InputDeviceID,OutputDeviceID) -> IORef DisplayInfo -> IdleCallback
idle     startTimeRef     (inputID, outputID)              displayInfoRef = do
    difference <- computePassedTime startTimeRef
    displayInfo <- readIORef displayInfoRef 
    newDisplayInfo <- updateSongInfo (displayInfo {notePlace = placeNoteToBePlayed difference}) startTimeRef
    displayInfoRef $= newDisplayInfo
    postRedisplay Nothing

display :: IORef DisplayInfo -> Difficulty -> DisplayCallback
display    displayInfoRef modus = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ renderAllTogether displayInfo modus
    swapBuffers
    flush

reshape :: ReshapeCallback
reshape (Size width height) = do
    viewport $= (Position ((width - 700) `div` 2)  ((height - 500) `div` 2), (Size 700 500))
    postRedisplay Nothing
    
setModusAndSong :: [String]    -> IO (Maybe Difficulty, Maybe Song)
setModusAndSong    (myModus : song : []) = do
    return $ (lookup myModus modi, lookup song songCollection)
setModusAndSong    _             = return (Nothing,Nothing)