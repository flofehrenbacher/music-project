{-# LANGUAGE FlexibleContexts #-}
module Main where

import Display
import DisplayInfo
import Idle
import Jingle
import Difficulty
import MouseEvents
import MidiFun
import SongCollection
import WindowSize

import Data.IORef
import Euterpea
import Graphics.UI.GLUT
import Text.Read

-- | starts the whole program
-- if difficulty and song were delivered correctly the song starts
-- otherwise the possibilities for difficulty and song will be printed to the console
main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    (difficulty, song)    <- setModeAndSong args
    case (difficulty, song) of
        (Just difficulty', Just song') -> startSong (difficulty',song') progName
        (_          , _         ) ->  do
            putStrLn $ "The first argument must be the difficulty: " ++ unwords getAllModi
            putStrLn "The second argument must be the song you want to learn" 
            putStrLn $ "You can choose one of the following: " ++ unwords allSongs

-- | starts the mainWindow, initializes the information according to the song,
-- initializes MidiDevices and callbacks
startSong :: (Difficulty, Song)                 -> String    -> IO()
startSong    (difficulty,song)                     progName  =  do
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize  $= Size initWidth initHeight
    createWindow progName
    (inputID, outputID) <- initDevices
    jingle outputID
    (displayInfoRef, startTimeRef) <- initializeDisplayInfo song
    mouseCallback   $= Just (mouse displayInfoRef outputID)
    idleCallback    $= Just (idle startTimeRef (inputID, outputID) displayInfoRef)
    displayCallback $= display displayInfoRef difficulty
    reshapeCallback $= Just reshape
    mainLoop

-- | checks if the list only contains two Strings and
-- returns the corresponding difficulty and Song, respectively
setModeAndSong :: [String]    -> IO (Maybe Difficulty, Maybe Song)
setModeAndSong    (myMode : song : []) = do
    return $ (readMaybe myMode, lookup song songCollection)
setModeAndSong    _             = return (Nothing,Nothing)