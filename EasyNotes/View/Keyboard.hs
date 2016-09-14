module View.Keyboard where

import Euterpea

import View.Clef
import View.Fun
import View.Text
import Modus
import View.Key
import View.NoteLine
import DisplayInfo

import Graphics.Rendering.OpenGL

-- | displays the keyboard according to the current displayInfo
-- depends on if the last note that was played was the right one,
-- what the next note is and what modus is chosen
renderAllTogether :: DisplayInfo -> Modus -> IO ()
renderAllTogether    displayInfo    modus =  do
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    let currentPitchClassPlayed = lastNote displayInfo
    let isKeyCurrentPressed = isKeyPressed displayInfo || isMidiKeyPressed displayInfo
    preservingMatrix $ drawLines
    preservingMatrix $ drawClef
    case songInfo displayInfo of
        -- song ends
        Nothing -> do
            preservingMatrix $ showTextAboveKeyboard "Song finished!" 0.3
            preservingMatrix $ renderKeyboard Nothing False False
        -- song continues
        Just (noteToBePlayed, restNotes) -> do
            if modus /= Hard then preservingMatrix $ showTextAboveKeyboard (fst (pitchInformation noteToBePlayed)) (notePlace displayInfo)
                else return ()
            preservingMatrix $ showNoteAboveKeyboard (pitchInformation noteToBePlayed) (notePlace displayInfo)
            preservingMatrix $ renderKeyboard currentPitchClassPlayed (isRightNotePlayed displayInfo) isKeyCurrentPressed
            if modus == Easy then preservingMatrix $ labelKeys
                else return ()

-- | displays one octave of a keyboard
renderKeyboard :: Maybe PitchClass     -> Bool           -> Bool   -> IO ()
renderKeyboard    currentPitchClassPlayed _                 False  = do
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ drawNKeys nextWhiteKey 6
    drawBlackKeys
renderKeyboard    currentPitchClassPlayed isRightNotePlayed True   = do
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
    
displayPressedKey :: Bool -> Maybe PitchClass  -> Bool -> IO ()
displayPressedKey    True   (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (findPlaceFor pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenKey
displayPressedKey    False  (Just  pitchClass)    True = preservingMatrix $ do
    translate$Vector3 (findPlaceFor pitchClass::GLfloat) 0 0
    renderPrimitive Quads redKey
displayPressedKey    True   (Just  pitchClass)    False = preservingMatrix $ do
    translate$Vector3 (findPlaceFor pitchClass::GLfloat) 0 0
    renderPrimitive Quads greenBlackKey
displayPressedKey    False   (Just  pitchClass)   False = preservingMatrix $ do
    translate$Vector3 (findPlaceFor pitchClass::GLfloat) 0 0
    renderPrimitive Quads redBlackKey
displayPressedKey    _       _                    _   = return ()