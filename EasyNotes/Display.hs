module Display where

import Graphics.UI.GLUT
import DisplayInfo
import Modi
import Data.IORef
import View.Keyboard

display :: IORef DisplayInfo -> Modus -> DisplayCallback
display    displayInfoRef modus = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ renderAllTogether displayInfo modus
    swapBuffers
    flush