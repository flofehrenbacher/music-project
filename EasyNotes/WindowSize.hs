-- | This module defines the reshapecallback for /Easy Notes/.
module WindowSize where

import Graphics.UI.GLUT

-- | initital width and height, respectivly
initHeight, initWidth :: GLint
initHeight = 500
initWidth  = 700

-- | Keeps the initial size of the display in the center of the window no matter how height or width are edited.
reshape :: ReshapeCallback
reshape (Size width height) = do
    viewport $= (Position ((width - initWidth) `div` 2)  ((height - initHeight) `div` 2), (Size initWidth initHeight))
    postRedisplay Nothing