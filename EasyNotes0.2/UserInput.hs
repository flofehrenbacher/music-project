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

getRestInfo :: [Music Pitch] -> Maybe (PitchClass, [Music Pitch])
getRestInfo    []            = Nothing
getRestInfo    ((Prim (Note _ ((pitchClass,_)))) : notes) = (Just (pitchClass, notes))
getRestInfo    ((Prim (Rest _)) : notes) = getRestInfo notes
getRestInfo    _            =  Nothing

updateSongInfo :: DisplayInfo ->       IORef UTCTime -> IO (DisplayInfo)
updateSongInfo    displayInfo _  | songInfo displayInfo == Nothing = return displayInfo
updateSongInfo    displayInfo startTimeRef  = do
            let noteToBePlayed = (fmap fst (songInfo displayInfo))
            let restNotes = (fmap snd (songInfo displayInfo))
            -- RIGHT NOTE WAS PLAYED
            if isAbsPitchTheSame noteToBePlayed (lastNote displayInfo) then do
                let displayInfo1 = displayInfo {greenPlace = findPlaceFor <$> noteToBePlayed, redPlace = Nothing, songInfo = getRestInfo =<< restNotes, lastNote = Nothing}
                curTime <- getCurrentTime
                startTimeRef $= curTime
                return displayInfo1
            -- WRONG NOTE WAS PLAYED
            else do
                case (lastNote displayInfo) of
                    Nothing -> return displayInfo
                    playedNote -> do
                        let displayInfo1 = displayInfo {greenPlace = Nothing, redPlace = findPlaceFor <$> playedNote}
                        return displayInfo1