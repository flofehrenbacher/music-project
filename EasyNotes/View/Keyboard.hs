module View.Keyboard where

import View.Fun
import View.Text
import Modus
import View.Key
import View.NoteLine
import DisplayInfo

import Data.IORef
import Euterpea
import Graphics.Rendering.OpenGL


renderAllTogether :: DisplayInfo -> Modus -> IO ()
renderAllTogether    displayInfo modus = do
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    let currentPitchClassPlayed = lastNote displayInfo
    case songInfo displayInfo of
        -- song ends
        Nothing -> do
            preservingMatrix $ showTextAboveKeyboard "Song finished!" 0.3
            preservingMatrix $ renderKeyboard Nothing Nothing
            preservingMatrix drawLines
        -- song continues
        Just (noteToBePlayed, restNotes) -> do
            if modus /= Hard then preservingMatrix $ showTextAboveKeyboard (fst (pitchInformation noteToBePlayed)) (notePlace displayInfo)
                else return ()
            preservingMatrix $ showNoteAboveKeyboard (pitchInformation noteToBePlayed) (notePlace displayInfo)
            preservingMatrix $ renderKeyboard (greenPlace displayInfo) (redPlace displayInfo)
            if modus == Easy then preservingMatrix $ labelKeys
                else return ()
            preservingMatrix $ drawLines

renderKeyboard :: Maybe GLfloat -> Maybe GLfloat -> IO ()
renderKeyboard    xRightNote       xWrongNote = do
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ drawNKeys nextWhiteKey 6
    colourWhiteKeyGreen xRightNote
    colourWhiteKeyRed xWrongNote
    drawBlackKeys
    colourBlackKeyGreen xRightNote
    colourBlackKeyRed xWrongNote

colourWhiteKeyGreen :: Maybe GLfloat -> IO ()
colourWhiteKeyGreen   (Just fl) | isWhiteKey fl = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads greenKey
colourWhiteKeyGreen    _                  = return ()

colourWhiteKeyRed :: Maybe GLfloat -> IO ()
colourWhiteKeyRed   (Just fl) | isWhiteKey fl = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads redKey
colourWhiteKeyRed    _                  = return ()

colourBlackKeyGreen :: Maybe GLfloat -> IO ()
colourBlackKeyGreen   (Just fl) | isWhiteKey fl == False = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads greenBlackKey
colourBlackKeyGreen    _                  = return ()

colourBlackKeyRed :: Maybe GLfloat -> IO ()
colourBlackKeyRed   (Just fl) | isWhiteKey fl == False = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads redBlackKey
colourBlackKeyRed    _                  = return ()

isWhiteKey :: GLfloat -> Bool
isWhiteKey    x       = x == fromInteger (round x)