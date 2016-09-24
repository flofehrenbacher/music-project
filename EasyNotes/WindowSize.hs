module WindowSize where

import Graphics.UI.GLUT

initHeight, initWidth :: GLint
initHeight = 500
initWidth  = 700

reshape :: ReshapeCallback
reshape (Size width height) = do
    viewport $= (Position ((width - initWidth) `div` 2)  ((height - initHeight) `div` 2), (Size initWidth initHeight))
    postRedisplay Nothing