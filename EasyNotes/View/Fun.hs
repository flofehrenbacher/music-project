-- | This module contains functions which make it a lot easier to render primitive shapes
module View.Fun where

import Graphics.UI.GLUT

-- | Renders a list of vertices (described by x-, y- and z-coordinate)
-- as desired primitive shape
renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure$makeVertexes ps
    where makeVertexes :: [(GLfloat, GLfloat, GLfloat)] -> IO()
          makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)