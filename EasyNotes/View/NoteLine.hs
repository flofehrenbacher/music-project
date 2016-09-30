-- | This module defines functions to display the lines of the note line
module View.NoteLine where

import View.Fun
import Graphics.UI.GLUT

-- | Displays a helping line for the notes from C until D sharp
helpingLine :: IO ()
helpingLine = do
    currentColor $= Color4 1 1 1 1
    lineWidth $= 1
    renderAs Lines helpingLinePoints
        where helpingLinePoints :: [(GLfloat, GLfloat, GLfloat)]
              helpingLinePoints = [(-0.01,1.2,0),
                                   (0.06,1.2,0)]

-- | Displays the five lines of the note line
drawLines :: IO ()
drawLines = do
    currentColor $= Color4 1 1 1 1
    lineWidth $= 1
    translate$Vector3 (0::GLfloat) (1.25) 0
    renderAs Lines myLine
    nextLine
    nextLine
    nextLine
    nextLine
        where myLine :: [(GLfloat, GLfloat, GLfloat)]
              myLine = [(-0.2,0,0),
                        (1.6,0,0)]
              nextLine :: IO ()
              nextLine = do
                    translate$Vector3 (0::GLfloat) 0.05 0
                    renderAs Lines myLine