module DisplayInfo where

import Euterpea
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MidiIO
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Time.Clock

import SongCollection
import View.Text

type GreenPlace = Maybe GLfloat
type RedPlace = Maybe GLfloat
type LastNote = Maybe PitchClass
type SongInfo = Maybe (PitchClass, [Music Pitch])
type NotePlace = GLfloat
type MyTime = IORef UTCTime
data DisplayInfo = DisplayInfo {greenPlace :: GreenPlace, redPlace :: RedPlace, songInfo :: SongInfo, lastNote :: LastNote, notePlace :: NotePlace}
    deriving (Eq,Show)

setUpDisplayInfo :: Song -> IO (IORef DisplayInfo, IORef UTCTime)
setUpDisplayInfo    song = do
    displayInfoRef <- newIORef $ DisplayInfo {greenPlace = Nothing, redPlace = Nothing, songInfo = getRestInfo song, lastNote = Nothing, notePlace = 1.4}
    curTime <- getCurrentTime
    startTimeRef <- newIORef curTime
    return (displayInfoRef,startTimeRef)

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
                let newDisplayInfo = displayInfo {greenPlace = findPlaceFor <$> noteToBePlayed, redPlace = Nothing, songInfo = getRestInfo =<< restNotes, lastNote = Nothing}
                curTime <- getCurrentTime
                startTimeRef $= curTime
                return newDisplayInfo
            -- WRONG NOTE WAS PLAYED
            else do
                case (lastNote displayInfo) of
                    Nothing -> return displayInfo
                    playedNote -> do
                        return $ displayInfo {greenPlace = Nothing, redPlace = findPlaceFor <$> playedNote}

findPlaceFor :: PitchClass -> GLfloat
findPlaceFor    C          =  0
findPlaceFor    D          =  1
findPlaceFor    E          =  2
findPlaceFor    F          =  3
findPlaceFor    G          =  4
findPlaceFor    A          =  5
findPlaceFor    B          =  6
findPlaceFor    Cs         =  0.75
findPlaceFor    Df         =  0.75
findPlaceFor    Ds         =  1.75
findPlaceFor    Ef         =  1.75
findPlaceFor    Ff         =  2
findPlaceFor    Es         =  3
findPlaceFor    Fs         =  3.75
findPlaceFor    Gf         =  3.75
findPlaceFor    Gs         =  4.75
findPlaceFor    Af         =  4.75
findPlaceFor    As         =  5.75
findPlaceFor    Bf         =  5.75
findPlaceFor    Bs         =  0
findPlaceFor    _          =  10 -- for the moment