module DisplayInfo where

import SongCollection
import Songs.AuxilaryFunctions
import Time
import View.Text
import View.Key

import Data.IORef
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT

initialNotePlace :: GLfloat
initialNotePlace = 1.4

-- | contains all the information which is needed to display the current state
data DisplayInfo = DisplayInfo {isRightNotePlayed :: Bool,
                                isScreenKeyPressed :: Bool,
                                isMidiKeyPressed :: Bool,
                                songInfo :: Maybe (PitchClass, [Music Pitch]),
                                currentNote :: Maybe PitchClass,
                                lastNote :: Maybe PitchClass,
                                notePlace :: GLfloat}
                                  deriving (Eq,Show)

-- | returns the initial information to display the initial state according to the passed song
initializeDisplayInfo :: Song -> IO (IORef DisplayInfo, IORef UTCTime)
initializeDisplayInfo    song = do
    displayInfoRef <- newIORef $ DisplayInfo {isRightNotePlayed = False,
                                              isScreenKeyPressed = False,
                                              isMidiKeyPressed = False,
                                              songInfo = updateSongInfo song,
                                              lastNote = Nothing,
                                              currentNote = Nothing,
                                              notePlace = initialNotePlace}
    curTime        <- getCurrentTime
    startTimeRef   <- newIORef curTime
    return (displayInfoRef,startTimeRef)

-- | updates the song information, speaking returns a tuple containing the next pitch class which has to be played
-- and a list of the remaining notes of the song
-- if the song has finished returns Nothing
updateSongInfo :: Song                                      -> Maybe (PitchClass, [Music Pitch])
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
            if (isScreenKeyPressed displayInfo' || isMidiKeyPressed displayInfo') && isAbsPitchTheSame nextNote (lastNote displayInfo') then do
                let newDisplayInfo = displayInfo' {isRightNotePlayed = True,
                                                  songInfo = updateSongInfo =<< remainingNotes,
                                                  lastNote = Nothing}
                resetTime startTimeRef
                return newDisplayInfo
            -- WRONG NOTE WAS PLAYED
            else do
                case (isScreenKeyPressed displayInfo' || isMidiKeyPressed displayInfo') of
                    False -> return displayInfo' {isRightNotePlayed = False}
                    True -> do
                        return $ displayInfo'

-- | updates in the display information the last note that was played
updateLastNote ::  Maybe PitchClass -> DisplayInfo     -> IO DisplayInfo
updateLastNote     (Just pitchClass)   displayInfo = do
            return (displayInfo {lastNote = Just pitchClass, currentNote = Just pitchClass})
updateLastNote     _                   displayInfo = return displayInfo

-- | updates in the display information if a key is currently pressed on the screen
keyPressed :: IORef DisplayInfo -> Bool    -> IO ()
keyPressed    displayInfoRef       press   = do
    displayInfo <- readIORef displayInfoRef
    let newDisplayInfo = displayInfo {isScreenKeyPressed = press}
    displayInfoRef $= newDisplayInfo