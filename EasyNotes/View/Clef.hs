-- | This module contains functions to draw a simple treble clef
module View.Clef where

import View.Fun
import Graphics.UI.GLUT

-- | Draws a simple treble clef to the canvas
drawClef :: IO ()
drawClef = do
    translate$Vector3 (-0.1::GLfloat) (1.25) 0
    scale 0.4 0.4 (0::GLfloat)
    currentColor $= Color4 1 1 1 1
    lineWidth $= 2
    renderAs LineStrip clefPoints
          where clefPoints :: [(GLfloat, GLfloat, GLfloat)]
                clefPoints = [(-0.1,-0.2,0), (0,-0.2,0), (0,0.7,0), (0.1,0.7,0),
                        (0.1,0.47,0), (-0.15,0.3,0), (-0.15,0.03,0),
                        (0.15,0.03,0), (0.15,0.2,0), (-0.1,0.2,0)]