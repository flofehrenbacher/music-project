module View.Keyboard where

import Euterpea

import DisplayInfo
import Difficulty
import PitchClass
import View.Clef
import View.Fun
import View.Text
import View.Key
import View.NoteLine

import Graphics.Rendering.OpenGL

xCoordCs, xCoordFs :: GLfloat
xCoordCs = 0.75
xCoordFs = 3.75

-- | displays the keyboard according to the current displayInfo
-- depends on if the last note that was played was the right one,
-- on which the next note is and on which difficulty is chosen
displayAllTogether :: DisplayInfo -> Difficulty -> IO ()
displayAllTogether    displayInfo    difficulty =  do
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    let isKeyCurrentPressed = isScreenKeyPressed displayInfo || isMidiKeyPressed displayInfo
    preservingMatrix $ drawLines
    preservingMatrix $ drawClef
    case songInfo displayInfo of
        -- SONG FINISHED
        Nothing -> do
            preservingMatrix $ displayTextAboveKeyboard "Song finished!" 0.3
            preservingMatrix $ displayKeyboard Nothing False False
        -- SONG CONTINUES
        Just (noteToBePlayed, restNotes) -> do
            if difficulty /= Hard then preservingMatrix $ displayTextAboveKeyboard (fst (pitchInformation noteToBePlayed)) (notePlace displayInfo)
                else return ()
            preservingMatrix $ displayNoteAboveKeyboard (pitchInformation noteToBePlayed) (notePlace displayInfo)
            preservingMatrix $ displayKeyboard (currentNote displayInfo) (isRightNotePlayed displayInfo) isKeyCurrentPressed
            if difficulty == Easy then preservingMatrix $ labelKeys
                else return ()

-- | displays one octave of a keyboard including the possibly currently played key
displayKeyboard :: Maybe PitchClass     -> Bool           -> Bool   -> IO ()
displayKeyboard    currentPitchClassPlayed _                 False  = do
    -- NO KEY PLAYED AT THE MOMENT
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ drawNKeys nextWhiteKey 6
    drawBlackKeys
displayKeyboard    currentPitchClassPlayed isRightNotePlayed True   = do
    -- KEY PLAYED AT THE MOMENT
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ drawNKeys nextWhiteKey 6
    let white = fmap isWhiteKey currentPitchClassPlayed
    if  white == Just True then do
        displayPressedKey isRightNotePlayed currentPitchClassPlayed True
        else return ()
    drawBlackKeys
    if white == Just False then do
        displayPressedKey isRightNotePlayed currentPitchClassPlayed False
        else return ()

-- | display the current played key in colour: green if it was the right one, red if it was the wrong one
displayPressedKey :: Bool -> Maybe PitchClass  -> Bool -> IO ()
displayPressedKey    True   (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenKey
displayPressedKey    False  (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads redKey
displayPressedKey    True   (Just  pitchClass)    False = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenBlackKey
displayPressedKey    False   (Just  pitchClass)   False = preservingMatrix $ do
    translate$Vector3 (xCoordOfPitchClass pitchClass::GLfloat) 0 0
    renderPrimitive Quads redBlackKey
displayPressedKey    _       _                    _   = return ()

-- | computes the x coordinate of the given pitch class
xCoordOfPitchClass :: PitchClass -> GLfloat
xCoordOfPitchClass    pitchClass | isWhiteKey pitchClass = heightMainNotes pitchClass
xCoordOfPitchClass    pitchClass | pitchClass < E        = xCoordCs + (realToFrac (((absPitch (pitchClass,4)) `mod` 61))) / 2
xCoordOfPitchClass    pitchClass | otherwise             = xCoordFs + (realToFrac (((absPitch (pitchClass,4)) `mod` 66))) / 2