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

-- | Starts the whole program.
--
-- If 'Difficulty' and 'Song' were delivered correctly the program will be executed.
-- Otherwise the possibilities for 'Difficulty' and 'Song' will be printed to the console.
main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    (difficulty, song)    <- setModeAndSong args
    case (difficulty, song) of
        (Just difficulty', Just song') -> startSong (difficulty',song') progName
        (_          , _         ) ->  do
            putStrLn $ "The first argument must be the difficulty: " ++ unwords getAllModes
            putStrLn "The second argument must be the song you want to learn" 
            putStrLn $ "You can choose one of the following: " ++ unwords allSongs

-- | Starts the main window, initializes the 'displayInfo' according to the 'Song',
-- initializes the input and output midi devices and the callbacks.
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

-- | Checks whether the list only contains two 'String's and
-- returns the corresponding 'Difficulty' and 'Song', respectively.
setModeAndSong :: [String]    -> IO (Maybe Difficulty, Maybe Song)
setModeAndSong    (myMode : song : []) = do
    return $ (readMaybe myMode, lookup song songCollection)
setModeAndSong    _             = return (Nothing,Nothing)