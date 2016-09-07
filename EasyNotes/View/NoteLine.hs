module View.NoteLine where

import Graphics.UI.GLUT
import View.Clef

helpingLine = do
    currentColor $= Color4 1 1 1 1
    lineWidth $= 1
    renderPrimitive Lines help

drawLines :: TextureObject -> IO ()
drawLines clef = do
    drawClef clef
    currentColor $= Color4 1 1 1 1
    lineWidth $= 1
    translate$Vector3 (0::GLfloat) (1.25) 0
    renderPrimitive Lines myLines2
    nextLine
    nextLine
    nextLine
    nextLine

myLines2 = do
    vertex$Vertex3 ((-0.2)::GLfloat) 0 0
    vertex$Vertex3 (1.6::GLfloat) 0 0
    
help = do
    vertex$Vertex3 ((-0.01)::GLfloat) 1.2 0
    vertex$Vertex3 (0.06::GLfloat) 1.2 0

nextLine = do
    translate$Vector3 (0::GLfloat) 0.05 0
    renderPrimitive Lines myLines2