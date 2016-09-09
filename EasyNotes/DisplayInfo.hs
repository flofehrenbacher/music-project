module DisplayInfo where

import Data.IORef
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT

import SongCollection
import Songs.AuxilaryFunctions
import Time
import View.Text

data DisplayInfo = DisplayInfo {greenPlace :: Maybe GLfloat,
                                redPlace :: Maybe GLfloat,
                                isKeyPressed :: Bool,
                                songInfo :: Maybe (PitchClass, [Music Pitch]),
                                lastNote :: Maybe PitchClass,
                                notePlace :: GLfloat}
                                  deriving (Eq,Show)

initializeDisplayInfo :: Song -> IO (IORef DisplayInfo, IORef UTCTime)
initializeDisplayInfo    song = do
    displayInfoRef <- newIORef $ DisplayInfo {greenPlace = Nothing,
                                              redPlace = Nothing,
                                              isKeyPressed = False,
                                              songInfo = updateSongInfo song,
                                              lastNote = Nothing,
                                              notePlace = 1.4}
    curTime        <- getCurrentTime
    startTimeRef   <- newIORef curTime
    return (displayInfoRef,startTimeRef)

-- TODO nicht restInfo und nicht get
updateSongInfo :: [Music Pitch]                              -> Maybe (PitchClass, [Music Pitch])
updateSongInfo    []                                         =  Nothing
updateSongInfo    ((Prim (Note _ ((pitchClass,_)))) : notes) =  (Just (pitchClass, notes))
updateSongInfo    ((Prim (Rest _)) : notes)                  =  updateSongInfo notes
updateSongInfo    _                                          =  Nothing

-- besser strukturieren / seperieren mit maybe / time
updateDisplayInfo :: DisplayInfo ->  IORef UTCTime                                    -> IO (DisplayInfo)
updateDisplayInfo    displayInfo     _              | songInfo displayInfo == Nothing = return displayInfo
updateDisplayInfo    displayInfo     startTimeRef                                     = do
            let nextNote = (fmap fst (songInfo displayInfo))
            let remainingNotes = (fmap snd (songInfo displayInfo))
            -- RIGHT NOTE WAS PLAYED
            if isAbsPitchTheSame nextNote (lastNote displayInfo) then do
                let newDisplayInfo = displayInfo {greenPlace = findPlaceFor <$> nextNote, 
                                                  redPlace = Nothing,
                                                  songInfo = updateSongInfo =<< remainingNotes,
                                                  lastNote = Nothing}
                resetTime startTimeRef
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