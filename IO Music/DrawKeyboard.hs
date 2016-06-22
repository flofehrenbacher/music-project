module DrawKeyboard where

import DrawFun
import DrawText

import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

whiteKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
whiteKeyPoints = [(0,0,0), (0.95,0,0), (0.95,4,0), (0,4,0)]

blackKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
blackKeyPoints = [(0.75, 1.5, 0), (1.25, 1.5 ,0), (1.25, 3.95, 0), (0.75, 3.95, 0)]

keyboardFigure :: Maybe GLfloat -> Maybe GLfloat -> IO ()
keyboardFigure xRightNote xFalseNote = do
    scale 0.2 0.2 (0::GLfloat)
    renderAs Quads whiteKeyPoints
    preservingMatrix $ drawNKeys nextWhiteKey 6
    case xRightNote of
        Just fl -> do
                preservingMatrix $ do
                    translate$Vector3 (fl::GLfloat) 0 0
                    renderPrimitive Quads greenKey
        Nothing -> return ()
    case xFalseNote of
        Just fl -> do
                preservingMatrix $ do
                    translate$Vector3 (fl::GLfloat) 0 0
                    renderPrimitive Quads redKey
        Nothing -> return ()
    currentColor $= Color4 0 0 0 1
    renderAs Quads blackKeyPoints
    preservingMatrix $ nextBlackKey >> translate (Vector3 (1::GLfloat) 0 0) >> drawNKeys nextBlackKey 3
    
findPlaceFor :: PitchClass -> Maybe GLfloat
findPlaceFor    C          = Just 0
findPlaceFor    D          = Just 1
findPlaceFor    E          = Just 2
findPlaceFor    F          = Just 3
findPlaceFor    G          = Just 4
findPlaceFor    A          = Just 5
findPlaceFor    B          = Just 6
findPlaceFor    _          = Nothing -- for the moment
    
    
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