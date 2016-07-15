module UserInput where

import View.Keyboard
import View.Text
import Types

import Data.Time.Clock
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Euterpea
import Data.IORef
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

isAbsPitchTheSame :: Maybe PitchClass -> Maybe PitchClass -> Bool
isAbsPitchTheSame   (Just  pcOne)       (Just pcTwo)      | pcToInt pcOne == pcToInt pcTwo = True
                                                          | otherwise = False
isAbsPitchTheSame   _                   _                 = False

updateLastPlayedNote :: LastNote    -> Maybe (PitchClass)     -> IO()
updateLastPlayedNote    lastPlayedNote (Just pc)              = lastPlayedNote $= Just pc
updateLastPlayedNote    _              _                      = return ()

getRestInfo :: [Music Pitch] -> IO (Maybe (PitchClass, [Music Pitch]))
getRestInfo    []            = return Nothing
getRestInfo    ((Prim (Note _ ((pitchClass,_)))) : notes) = do
    return (Just (pitchClass, notes))
getRestInfo    ((Prim (Rest _)) : notes) = do
    getRestInfo notes
getRestInfo    _            = do
    return Nothing

updateSongInfo :: Maybe (PitchClass) -> Maybe (PitchClass, [Music Pitch])  -> GreenPlace -> RedPlace -> SongInfo -> IORef UTCTime -> IO()
updateSongInfo    _                     Nothing                               _             _           _           _             = return ()
updateSongInfo    currentPitchClassPlayed (Just (noteToBePlayed ,restNotes))  greenRef      redRef      songInfoRef startTimeRef  = do
            -- RIGHT NOTE WAS PLAYED
            if isAbsPitchTheSame (Just noteToBePlayed) currentPitchClassPlayed then do
                greenRef $= findPlaceFor noteToBePlayed
                redRef $= Nothing
                songInfo <- getRestInfo restNotes
                songInfoRef $= songInfo
                curTime <- getCurrentTime
                startTimeRef $= curTime
            -- WRONG NOTE WAS PLAYED
            else do
                case currentPitchClassPlayed of
                    Nothing -> return ()
                    Just pc -> do 
                        redRef $= findPlaceFor pc
                        greenRef $= Nothing