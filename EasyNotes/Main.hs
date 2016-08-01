{-# LANGUAGE FlexibleContexts #-}
module Main where

import Idle
import Modi
import MouseEvents
import DisplayInfo
import View.Keyboard
import SongCollection
import MidiFun

import Data.IORef
import Graphics.UI.GLUT
import Text.Read

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    arguments <- setModusAndSong args
    startWithArguments arguments progName

display :: IORef DisplayInfo -> Modus -> DisplayCallback
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

-- startWithArguments :: (Maybe Modus, Maybe Song) -> IO()
startWithArguments    (Just myModus, Just song)  progName       = do
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 700 500
    createWindow progName
    (displayInfoRef, startTimeRef) <- setUpDisplayInfo song
    (inputID, outputID) <- initDevices
    mouseCallback   $= Just (mouse displayInfoRef outputID)
    idleCallback    $= Just    (idle startTimeRef (inputID, outputID) displayInfoRef)
    displayCallback $= display displayInfoRef myModus
    reshapeCallback $= Just reshape
    mainLoop
startWithArguments  _                 _                   = do
    putStrLn "The first argument must be: easy, medium or hard"
    putStrLn "The second one must be the song you want to learn" 
    putStrLn "AlleMeineEntchen, HaenschenKlein, MadWorld, AllNotes"

setModusAndSong :: [String]    -> IO (Maybe Modus, Maybe Song)
setModusAndSong    (myModus : song : []) = do
    return $ (readMaybe myModus, lookup song songCollection)
setModusAndSong    _             = return (Nothing,Nothing)
