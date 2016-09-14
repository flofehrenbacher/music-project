module DisplayInfo where

import Data.IORef
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT

import SongCollection
import Songs.AuxilaryFunctions
import Time
import View.Text
import View.Key

xCsBlackKey = 0.75
xFsBlackKey = 3.75

-- | DisplayInfo contains all the information corresponding to the song which is needed to display the current state
data DisplayInfo = DisplayInfo {isRightNotePlayed :: Bool,
                                isKeyPressed :: Bool,
                                isMidiKeyPressed :: Bool,
                                songInfo :: Maybe (PitchClass, [Music Pitch]),
                                lastNote :: Maybe PitchClass,
                                notePlace :: GLfloat}
                                  deriving (Eq,Show)

-- | returns the initial information to display the initial state according to the passed song
initializeDisplayInfo :: Song -> IO (IORef DisplayInfo, IORef UTCTime)
initializeDisplayInfo    song = do
    displayInfoRef <- newIORef $ DisplayInfo {isRightNotePlayed = False,
                                              isKeyPressed = False,
                                              isMidiKeyPressed = False,
                                              songInfo = updateSongInfo song,
                                              lastNote = Nothing,
                                              notePlace = 1.4}
    curTime        <- getCurrentTime
    startTimeRef   <- newIORef curTime
    return (displayInfoRef,startTimeRef)

-- | updates the song information, speaking returns a tuple containing the next pitch class which has to be played
-- and a list of the remaining notes of the song
-- if the song has finished returns Nothing
updateSongInfo :: [Music Pitch]                              -> Maybe (PitchClass, [Music Pitch])
updateSongInfo    []                                         =  Nothing
updateSongInfo    ((Prim (Note _ ((pitchClass,_)))) : notes) =  (Just (pitchClass, notes))
updateSongInfo    ((Prim (Rest _)) : notes)                  =  updateSongInfo notes
updateSongInfo    _                                          =  Nothing

-- besser strukturieren / seperieren mit maybe / time
updateDisplayInfo :: DisplayInfo ->  IORef UTCTime  -> Maybe PitchClass                                   -> IO (DisplayInfo)
updateDisplayInfo    displayInfo     _                 _                | songInfo displayInfo == Nothing = return displayInfo
updateDisplayInfo    displayInfo     startTimeRef      currentPitchClassPlayed                            = do
            let nextNote = (fmap fst (songInfo displayInfo))
            let remainingNotes = (fmap snd (songInfo displayInfo))
            displayInfo' <- updateLastNote currentPitchClassPlayed displayInfo
            -- RIGHT NOTE WAS PLAYED
            if (isKeyPressed displayInfo) && isAbsPitchTheSame nextNote (lastNote displayInfo') then do
                let newDisplayInfo = displayInfo' {isRightNotePlayed = True,
                                                  songInfo = updateSongInfo =<< remainingNotes}
                resetTime startTimeRef
                return newDisplayInfo
            -- WRONG NOTE WAS PLAYED
            else do
                case (isKeyPressed displayInfo) of
                    False -> return displayInfo' {isRightNotePlayed = False}
                    True -> do
                        return $ displayInfo'

updateLastNote ::  Maybe PitchClass -> DisplayInfo     -> IO DisplayInfo
updateLastNote     (Just pitchClass)   midiDisplayInfo = do
            return (midiDisplayInfo {lastNote = Just pitchClass})
updateLastNote     _                   midiDisplayInfo = return midiDisplayInfo

findPlaceFor :: PitchClass -> GLfloat
findPlaceFor    pitchClass | isWhiteKey pitchClass = heightMainNotes pitchClass
findPlaceFor    pitchClass | pitchClass < E        = xCsBlackKey + (realToFrac (((absPitch (pitchClass,4)) `mod` 61))) / 2
findPlaceFor    pitchClass | otherwise             = xFsBlackKey + (realToFrac (((absPitch (pitchClass,4)) `mod` 66))) / 2