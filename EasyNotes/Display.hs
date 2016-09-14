module Display where

import Graphics.UI.GLUT
import DisplayInfo
import Modus
import Data.IORef
import View.Keyboard

-- | does essential steps for displaying the window: clear, load identity, rendering, swap buffer and flush
display :: IORef DisplayInfo -> Modus -> DisplayCallback
display    displayInfoRef modus = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ displayAllTogether displayInfo modus
    swapBuffers
    flush