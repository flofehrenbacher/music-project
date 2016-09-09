{-# LANGUAGE FlexibleContexts #-}
module Main where

import Display
import DisplayInfo
import Idle
import Jingle
import Modus
import MouseEvents
import MidiFun
import SongCollection
import WindowSize

import Data.IORef
import Euterpea
import Graphics.UI.GLUT
import Text.Read

-- | starts the whole program
-- if modus and song were delivered correctly the song starts
-- otherwise the possibilities for modus and song will be printed to the console
main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    (modus, song)    <- setModusAndSong args
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
    (inputID, outputID) <- initDevices
    jingle outputID
    (displayInfoRef, startTimeRef) <- setUpDisplayInfo song
    mouseCallback   $= Just (mouse displayInfoRef outputID)
    idleCallback    $= Just (idle startTimeRef (inputID, outputID) displayInfoRef)
    displayCallback $= display displayInfoRef modus
    reshapeCallback $= Just reshape
    mainLoop

-- | checks if only two arguments were delivered
-- and returns them
setModusAndSong :: [String]    -> IO (Maybe Modus, Maybe Song)
setModusAndSong    (myModus : song : []) = do
    return $ (readMaybe myModus, lookup song songCollection)
setModusAndSong    _             = return (Nothing,Nothing)