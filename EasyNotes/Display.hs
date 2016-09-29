-- | This module defines the displaycallback for Easy Notes
module Display where

import Graphics.UI.GLUT
import DisplayInfo
import Difficulty
import Data.IORef
import View.Keyboard

-- | does essential steps for displaying the window: clear, load identity, rendering, swap buffer and flush
display :: IORef DisplayInfo -> Difficulty -> DisplayCallback
display    displayInfoRef difficulty = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ displayAllTogether displayInfo difficulty
    swapBuffers
    flush