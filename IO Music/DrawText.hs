module DrawText where

import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

showNotesOfList :: [Music Pitch] -> GLfloat -> IO ()
showNotesOfList    []               x  = return ()
showNotesOfList    ((Prim (Note _ ((pitchClass,_)))) : notes) x = do
    showTextAboveKeyboard (pitchToString pitchClass) x
showNotesOfList    _          _   = do
    showTextAboveKeyboard "ERROR" 0

pitchToString :: PitchClass -> String
pitchToString    C          = "C"
pitchToString    D          = "D"
pitchToString    E          = "E"
pitchToString    F          = "F"
pitchToString    G          = "G"
pitchToString    A          = "A"
pitchToString    B          = "B"
pitchToString    _          = "ich bin eine schwarze Taste"

showTextAboveKeyboard :: String -> GLfloat -> IO()
showTextAboveKeyboard    text x = preservingMatrix $ do
        translate$Vector3 (x+0.6::GLfloat) 1 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 6
        scale 0.0022 0.0022 (0::GLfloat)
        renderString Roman text