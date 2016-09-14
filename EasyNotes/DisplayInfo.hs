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

-- | contains all the information which is needed to display the current state
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
            if (isKeyPressed displayInfo' || isMidiKeyPressed displayInfo') && isAbsPitchTheSame nextNote (lastNote displayInfo') then do
                let newDisplayInfo = displayInfo' {isRightNotePlayed = True,
                                                  songInfo = updateSongInfo =<< remainingNotes}
                resetTime startTimeRef
                return newDisplayInfo
            -- WRONG NOTE WAS PLAYED
            else do
                case (isKeyPressed displayInfo' || isMidiKeyPressed displayInfo') of
                    False -> return displayInfo' {isRightNotePlayed = False}
                    True -> do
                        return $ displayInfo'

-- | updates the last note that was played in the display information
updateLastNote ::  Maybe PitchClass -> DisplayInfo     -> IO DisplayInfo
updateLastNote     (Just pitchClass)   displayInfo = do
            return (displayInfo {lastNote = Just pitchClass})
updateLastNote     _                   displayInfo = return displayInfo