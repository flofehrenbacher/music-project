module Display where

import Graphics.UI.GLUT
import DisplayInfo
import Modus
import Data.IORef
import View.Keyboard

-- | does essential steps for rendering the window: clear, load identity, rendering, swap buffer and flush
display :: IORef DisplayInfo -> Modus -> DisplayCallback
display    displayInfoRef modus = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ renderAllTogether displayInfo modus
    swapBuffers
    flush