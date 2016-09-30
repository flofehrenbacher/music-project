-- | This module defines functions for rendering the keyboard on the canvas
module View.Keyboard where

import DisplayInfo
import Difficulty
import PitchClass
import View.Clef
import View.Fun
import View.Text
import View.Key
import View.NoteLine

import Euterpea
import Graphics.Rendering.OpenGL

-- | These are the x-coordinates at which the keys of C sharp and F sharp start, respectively
xCoordCs, xCoordFs :: GLfloat
xCoordCs = 0.75
xCoordFs = 3.75

-- | Displays the keyboard according to the current state of the 'DisplayInfo'
-- 
-- This depends on if the last note that was played was the right one,
-- on which the next note is and on which difficulty is chosen.
displayAllTogether :: DisplayInfo -> Difficulty -> IO ()
displayAllTogether    displayInfo    difficulty =  do
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    let isKeyCurrentPressed = isScreenKeyPressed displayInfo || isMidiKeyPressed displayInfo
    preservingMatrix $ drawLines
    preservingMatrix $ drawClef
    case songInfo displayInfo of
        -- SONG FINISHED
        Nothing -> do
            preservingMatrix $ displayNoteName "Song finished!" 0.3
            preservingMatrix $ displayKeyboard Nothing False False
        -- SONG CONTINUES
        Just (noteToBePlayed, restNotes) -> do
            if difficulty /= Hard then preservingMatrix $ displayNoteName (fst (pitchInformation noteToBePlayed)) (notePlace displayInfo)
                else return ()
            preservingMatrix $ displayNoteAboveKeyboard (pitchInformation noteToBePlayed) (notePlace displayInfo)
            preservingMatrix $ displayKeyboard (currentNote displayInfo) (isRightNotePlayed displayInfo) isKeyCurrentPressed
            if difficulty == Easy then preservingMatrix $ labelKeys
                else return ()

-- | Displays one octave of a keyboard including the possibly currently played key
--
--   - the first 'Bool' is 'True' if the right note is played
--   - the second 'Bool' is 'True' if a key is pressed at the moment
displayKeyboard :: Maybe PitchClass     -> Bool           -> Bool   -> IO ()
displayKeyboard    currentPitchClassPlayed _                 False  = do
    -- NO KEY PLAYED AT THE MOMENT
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ makeNTimes nextWhiteKey 6
    displayBlackKeys
displayKeyboard    currentPitchClassPlayed isRightNotePlayed True   = do
    -- KEY PLAYED AT THE MOMENT
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ makeNTimes nextWhiteKey 6
    let white = fmap isWhiteKey currentPitchClassPlayed
    if  white == Just True then do
        displayPressedKey isRightNotePlayed currentPitchClassPlayed True
        else return ()
    displayBlackKeys
    if white == Just False then do
        displayPressedKey isRightNotePlayed currentPitchClassPlayed False
        else return ()

-- | Displays the current played key in colour: green if it was the right one and red if it was the wrong one
--
--   - the first 'Bool' is 'True' if the right key is played
--   - the second 'Bool' is 'True' if the key that is pressed is a white key
displayPressedKey :: Bool -> Maybe PitchClass  -> Bool -> IO ()
displayPressedKey    True   (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenWhiteKey
displayPressedKey    False  (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads redWhiteKey
displayPressedKey    True   (Just  pitchClass)    False = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenBlackKey
displayPressedKey    False   (Just  pitchClass)   False = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads redBlackKey
displayPressedKey    _       _                    _   = return ()

-- | Computes the x coordinate of the given pitch class at which the corresponding key starts
xCoordOfPitchClass :: PitchClass -> GLfloat
xCoordOfPitchClass    pitchClass | isWhiteKey pitchClass = heightMainNotes pitchClass
xCoordOfPitchClass    pitchClass | pitchClass < E        = xCoordCs + (realToFrac (((absPitch (pitchClass,4)) `mod` 61))) / 2
xCoordOfPitchClass    pitchClass | otherwise             = xCoordFs + (realToFrac (((absPitch (pitchClass,4)) `mod` 66))) / 2