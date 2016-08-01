module View.Keyboard where

import View.Fun
import View.Text
import Modi
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
colourWhiteKeyGreen   (Just fl) | isInt fl = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads greenKey
colourWhiteKeyGreen    _                  = return ()

colourWhiteKeyRed :: Maybe GLfloat -> IO ()
colourWhiteKeyRed   (Just fl) | isInt fl = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads redKey
colourWhiteKeyRed    _                  = return ()

colourBlackKeyGreen :: Maybe GLfloat -> IO ()
colourBlackKeyGreen   (Just fl) | isInt fl == False = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads greenBlackKey
colourBlackKeyGreen    _                  = return ()

colourBlackKeyRed :: Maybe GLfloat -> IO ()
colourBlackKeyRed   (Just fl) | isInt fl == False = preservingMatrix $ do
    translate$Vector3 (fl::GLfloat) 0 0
    renderPrimitive Quads redBlackKey
colourBlackKeyRed    _                  = return ()

isInt :: GLfloat -> Bool
isInt x = x == fromInteger (round x)

drawBlackKeys :: IO ()
drawBlackKeys = do
    currentColor $= Color4 0 0 0 1
    renderAs Quads blackKeyPoints
    preservingMatrix $ nextBlackKey >> translate (Vector3 (1::GLfloat) 0 0) >> drawNKeys nextBlackKey 3

nextBlackKey :: IO ()
nextBlackKey = do
    translate$Vector3 (1::GLfloat) 0 0
    renderAs Quads blackKeyPoints

nextWhiteKey :: IO ()
nextWhiteKey = do
    translate$Vector3 (1::GLfloat) 0 0
    renderAs Quads whiteKeyPoints
    
drawNKeys :: IO () -> Int -> IO ()
drawNKeys key 1 = key
drawNKeys key n = key >> drawNKeys  key (n - 1)

greenKey :: IO ()
greenKey = do
    currentColor $= Color4 0 1 0 1
    vertex$Vertex3 (0::GLfloat) 0 0
    vertex$Vertex3 (0.95::GLfloat) 0 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.95::GLfloat) 4 0
    vertex$Vertex3 (0::GLfloat) 4 0
    currentColor $= Color4 1 1 1 1

redKey :: IO ()
redKey = do
    currentColor $= Color4 1 0 0 1
    vertex$Vertex3 (0::GLfloat) 0 0
    vertex$Vertex3 (0.95::GLfloat) 0 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.95::GLfloat) 4 0
    vertex$Vertex3 (0::GLfloat) 4 0
    currentColor $= Color4 1 1 1 1

greenBlackKey :: IO ()
greenBlackKey = do
    currentColor $= Color4 0 1 0 1
    vertex$Vertex3 (0::GLfloat) 1.5 0
    vertex$Vertex3 (0.5::GLfloat) 1.5 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.5::GLfloat) 3.95 0
    vertex$Vertex3 (0::GLfloat) 3.95 0
    currentColor $= Color4 1 1 1 1

redBlackKey :: IO ()
redBlackKey = do
    currentColor $= Color4 1 0 0 1
    vertex$Vertex3 (0::GLfloat) 1.5 0
    vertex$Vertex3 (0.5::GLfloat) 1.5 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.5::GLfloat) 3.95 0
    vertex$Vertex3 (0::GLfloat) 3.95 0
    currentColor $= Color4 1 1 1 1

whiteKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
whiteKeyPoints = [(0,0,0), (0.95,0,0), (0.95,4,0), (0,4,0)]

blackKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
blackKeyPoints = [(0.75, 1.5, 0), (1.25, 1.5 ,0), (1.25, 3.95, 0), (0.75, 3.95, 0)]
