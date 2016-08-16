{-# LANGUAGE FlexibleContexts #-}
module Main where

import Idle
import Display
import WindowSize
import DisplayInfo
import Modus
import MouseEvents
import SongCollection
import MidiFun

import Data.IORef
import Graphics.UI.GLUT
import Text.Read

-- | starts the whole program
-- if modus and song were delivered correctly the song starts
-- otherwise the possibilities for modus and song will be printed to the console
main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    (modus, song) <- setModusAndSong args
    case (modus, song) of
        (Just modus', Just song') -> startSong (modus',song') progName
        (_          , _         ) ->  do
            putStrLn $ "The first argument must be the difficulty: " ++ unwords getAllModi
            putStrLn "The second argument must be the song you want to learn" 
            putStrLn $ "You can choose one of the following: " ++ unwords allSongs

-- | starts the mainWindow, initializes the information according to the song,
-- initializes MidiDevices and callbacks
startSong :: (Modus, Song)                 -> String    -> IO()
startSong    (modus,song)                     progName  =  do
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize  $= Size 700 500
    createWindow progName
    (displayInfoRef, startTimeRef) <- setUpDisplayInfo song
    (inputID, outputID) <- initDevices
    mouseCallback   $= Just (mouse displayInfoRef outputID)
    idleCallback    $= Just (idle startTimeRef (inputID, outputID) displayInfoRef)
    displayCallback $= display displayInfoRef modus
    reshapeCallback $= Just reshape
    mainLoop

-- | checks if only two arguments were delivered
setModusAndSong :: [String]    -> IO (Maybe Modus, Maybe Song)
setModusAndSong    (myModus : song : []) = do
    return $ (readMaybe myModus, lookup song songCollection)
setModusAndSong    _             = return (Nothing,Nothing)
