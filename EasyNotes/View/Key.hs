-- | This module defines functions concerning the keys on the keyboard
module View.Key where

import Euterpea
import View.Fun
import Graphics.UI.GLUT

-- | Displays all of the black keys of the one octave of a keyboard.
displayBlackKeys :: IO ()
displayBlackKeys = do
    currentColor $= Color4 0 0 0 1
    renderAs Quads blackKeyPoints
    preservingMatrix $ nextBlackKey >> translate (Vector3 (1::GLfloat) 0 0) >> makeNTimes nextBlackKey 3
          where nextBlackKey :: IO ()
                nextBlackKey = do
                    translate$Vector3 (1::GLfloat) 0 0
                    renderAs Quads blackKeyPoints

-- | execute an IO()-action n times
makeNTimes :: IO () -> Int -> IO ()
makeNTimes action 1 = action
makeNTimes action n = action >> makeNTimes  action (n - 1)

-- | Displays a white key shifted 1 on the x-axis
nextWhiteKey :: IO ()
nextWhiteKey = do
    translate$Vector3 (1::GLfloat) 0 0
    renderAs Quads whiteKeyPoints

-- | Defines the points for a white key.
whiteKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
whiteKeyPoints = [(0,0,0), (0.95,0,0), (0.95,4,0), (0,4,0)]

-- | Defines the points for a black key.
blackKeyPoints :: [(GLfloat, GLfloat, GLfloat)]
blackKeyPoints = [(0.75, 1.5, 0), (1.25, 1.5 ,0), (1.25, 3.95, 0), (0.75, 3.95, 0)]

-- | Displays a green /white/ key
greenWhiteKey :: IO ()
greenWhiteKey = do
    currentColor $= Color4 0 1 0 1
    vertex$Vertex3 (0::GLfloat) 0 0
    vertex$Vertex3 (0.95::GLfloat) 0 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.95::GLfloat) 4 0
    vertex$Vertex3 (0::GLfloat) 4 0
    currentColor $= Color4 1 1 1 1

-- | Displays a red /white/ key
redWhiteKey :: IO ()
redWhiteKey = do
    currentColor $= Color4 1 0 0 1
    vertex$Vertex3 (0::GLfloat) 0 0
    vertex$Vertex3 (0.95::GLfloat) 0 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.95::GLfloat) 4 0
    vertex$Vertex3 (0::GLfloat) 4 0
    currentColor $= Color4 1 1 1 1

-- | Displays a green /black/ key
greenBlackKey :: IO ()
greenBlackKey = do
    currentColor $= Color4 0 1 0 1
    vertex$Vertex3 (0::GLfloat) 1.5 0
    vertex$Vertex3 (0.5::GLfloat) 1.5 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.5::GLfloat) 3.95 0
    vertex$Vertex3 (0::GLfloat) 3.95 0
    currentColor $= Color4 1 1 1 1

-- | Displays a red /black/ key
redBlackKey :: IO ()
redBlackKey = do
    currentColor $= Color4 1 0 0 1
    vertex$Vertex3 (0::GLfloat) 1.5 0
    vertex$Vertex3 (0.5::GLfloat) 1.5 0
    currentColor $= Color4 1 1 1 1
    vertex$Vertex3 (0.5::GLfloat) 3.95 0
    vertex$Vertex3 (0::GLfloat) 3.95 0
    currentColor $= Color4 1 1 1 1


-- | Determines if the key corresponding to the PitchClass is white
isWhiteKey :: PitchClass       -> Bool
isWhiteKey    pitchClass       =  length (show pitchClass) == 1