module WindowSize where

import Graphics.UI.GLUT

reshape :: ReshapeCallback
reshape (Size width height) = do
    viewport $= (Position ((width - 700) `div` 2)  ((height - 500) `div` 2), (Size 700 500))
    postRedisplay Nothing