module DrawText where

import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

getRestInfo :: [Music Pitch] -> IO (Maybe (PitchClass, [Music Pitch]))
getRestInfo    []            = return Nothing
getRestInfo    ((Prim (Note _ ((pitchClass,_)))) : notes) = do
    return (Just (pitchClass, notes))
getRestInfo    ((Prim (Rest _)) : notes) = do
    getRestInfo notes
getRestInfo    _            = do
    return Nothing

placeNoteToBePlayed :: NominalDiffTime -> GLfloat
placeNoteToBePlayed difference = if difference < 6.3 then sin (realToFrac (difference) ) / 1.65 else 0

showTextAboveKeyboard :: String -> GLfloat -> IO()
showTextAboveKeyboard    text x = preservingMatrix $ do
        translate$Vector3 (x+0.6::GLfloat) 1 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 6
        scale 0.0022 0.0022 (0::GLfloat)
        renderString Roman text
        
pitchToString :: PitchClass -> String
pitchToString    C          = "C"
pitchToString    D          = "D"
pitchToString    E          = "E"
pitchToString    F          = "F"
pitchToString    G          = "G"
pitchToString    A          = "A"
pitchToString    B          = "B"
pitchToString    Cs         = "Cs"
pitchToString    Ds         = "Ds"
pitchToString    Es         = "Es"
pitchToString    Fs         = "Fs"
pitchToString    Gs         = "Gs"
pitchToString    As         = "As"
pitchToString    Bs         = "Bs"
pitchToString    Cf         = "Cb"
pitchToString    Df         = "Db"
pitchToString    Ef         = "Eb"
pitchToString    Ff         = "Fb"
pitchToString    Gf         = "Gb"
pitchToString    Af         = "Ab"
pitchToString    Bf         = "Bb"
pitchToString    _          = "unbekannt"