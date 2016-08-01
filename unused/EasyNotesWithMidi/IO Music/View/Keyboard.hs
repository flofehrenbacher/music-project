module View.Keyboard where

import View.Fun
import View.Text
import NoteLine
import Types

import Data.IORef
import Euterpea
import Graphics.Rendering.OpenGL


drawKeyboard :: DisplayInfo -> IO ()
drawKeyboard    (greenRef,redRef,songInfoRef,lastPlayedNote,placeForNote) = do
    translate$Vector3 (-0.7::GLfloat) (-0.7) 0
    x <- readIORef placeForNote
    currentPitchClassPlayed <- readIORef lastPlayedNote
    songInfo <- readIORef songInfoRef
    case songInfo of
        -- song ends
        Nothing -> do
            preservingMatrix $ showTextAboveKeyboard "Song finished!" (-0.4)
            preservingMatrix $ keyboardFigure Nothing Nothing
            preservingMatrix drawLines
        -- song continues
        Just (noteToBePlayed, restNotes) -> do
            preservingMatrix $ showTextAboveKeyboard (fst (pitchInformation noteToBePlayed)) x
            preservingMatrix $ showNoteAboveKeyboard (pitchInformation noteToBePlayed) x
            xRightNote <- readIORef greenRef
            xWrongNote <- readIORef redRef
            preservingMatrix $ keyboardFigure xRightNote xWrongNote
            preservingMatrix drawLines

keyboardFigure :: Maybe GLfloat -> Maybe GLfloat -> IO ()
keyboardFigure xRightNote xWrongNote = do
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
    
findPlaceFor :: PitchClass -> Maybe GLfloat
findPlaceFor    C          = Just 0
findPlaceFor    D          = Just 1
findPlaceFor    E          = Just 2
findPlaceFor    F          = Just 3
findPlaceFor    G          = Just 4
findPlaceFor    A          = Just 5
findPlaceFor    B          = Just 6
findPlaceFor    Cs         = Just 0.75
findPlaceFor    Df         = Just 0.75
findPlaceFor    Ds         = Just 1.75
findPlaceFor    Ef         = Just 1.75
findPlaceFor    Ff         = Just 2
findPlaceFor    Es         = Just 3
findPlaceFor    Fs         = Just 3.75
findPlaceFor    Gf         = Just 3.75
findPlaceFor    Gs         = Just 4.75
findPlaceFor    Af         = Just 4.75
findPlaceFor    As         = Just 5.75
findPlaceFor    Bf         = Just 5.75
findPlaceFor    Bs         = Just 0
findPlaceFor    _          = Nothing -- for the moment

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
