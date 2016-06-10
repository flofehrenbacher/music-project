module DisplayFun where

import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

playDisplayedNotes :: [Music Pitch] -> IO ()
playDisplayedNotes    []            = return ()
playDisplayedNotes    ((Prim (Note _ ((pitchClass,_)))) : notes) = do
    renderString MonoRoman (pitchToString pitchClass)
    print (pitchToString pitchClass)
    playDisplayedNotes notes
playDisplayedNotes    _             = do
    renderString MonoRoman "ERROR"

pitchToString :: PitchClass -> String
pitchToString    C          = "C"
pitchToString    D          = "D"
pitchToString    E          = "E"
pitchToString    F          = "F"
pitchToString    G          = "G"
pitchToString    A          = "A"
pitchToString    B          = "B"
pitchToString    _          = "ich bin eine schwarze Taste"