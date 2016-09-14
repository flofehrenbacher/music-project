module View.Key where

import Euterpea
import View.Fun
import Graphics.UI.GLUT

isWhiteKey :: PitchClass       -> Bool
isWhiteKey    pitchClass       =  length (show pitchClass) == 1

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