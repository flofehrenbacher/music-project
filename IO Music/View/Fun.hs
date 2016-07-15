module View.Fun where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

makeVertexes :: [(GLfloat, GLfloat, GLfloat)] -> IO()
makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure$makeVertexes ps