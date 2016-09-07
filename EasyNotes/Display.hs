module Display where

import Graphics.UI.GLUT
import DisplayInfo
import Modus
import Data.IORef
import View.Keyboard

display :: IORef DisplayInfo -> Modus -> TextureObject -> DisplayCallback
display    displayInfoRef modus clef = do
    clear [ColorBuffer]
    loadIdentity
    displayInfo <- readIORef displayInfoRef
    preservingMatrix $ renderAllTogether displayInfo modus clef
    swapBuffers
    flush