module DrawText where

import NoteLine

import Data.Text
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

showNoteAboveKeyboard :: (String,GLfloat) -> GLfloat -> IO()
showNoteAboveKeyboard    (noteName,height)   x = do
        translate$Vector3 (x+0.663::GLfloat) 0 0
        if height < 0 then preservingMatrix helpingLine else return ()
        translate$Vector3 0 (1.2285 + height::GLfloat ) 0
        lineWidth $= 2
        currentColor $= Color4 1 1 1 1
        scale 0.0007 0.00065 (0::GLfloat)
        showNote noteName
        
showTextAboveKeyboard :: String -> GLfloat -> IO()
showTextAboveKeyboard    text x = preservingMatrix $ do
        translate$Vector3 (x+0.64::GLfloat) 0.85 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 5
        scale 0.0011 0.0011 (0::GLfloat)
        renderString Roman text
    

showNote :: String -> IO()
showNote    noteName | takeEnd 1 (pack noteName) == pack "s" = renderString Roman "#o" 
showNote    noteName | takeEnd 1 (pack noteName) == pack "b" = renderString Roman "bo"
showNote    noteName | otherwise = renderString Roman "o"

pitchInformation :: PitchClass -> (String,GLfloat)
pitchInformation    C          =  ("C", (-0.05))
pitchInformation    Cs         =  ("Cs", (-0.05))
pitchInformation    Cf         =  ("Cb", (-0.05))
pitchInformation    D          =  ("D", (-0.025))
pitchInformation    Ds         =  ("Ds", (-0.025))
pitchInformation    Df         =  ("Db", (-0.025))
pitchInformation    E          =  ("E", 0)
pitchInformation    Es         =  ("Es", 0)
pitchInformation    Ef         =  ("Eb", 0)
pitchInformation    F          =  ("F", 0.025)
pitchInformation    Fs         =  ("Fs", 0.025)
pitchInformation    Ff         =  ("Fb", 0.025)
pitchInformation    G          =  ("G", 0.05)
pitchInformation    Gs         =  ("Gs", 0.05)
pitchInformation    Gf         =  ("Gb", 0.05)
pitchInformation    A          =  ("A", 0.075)
pitchInformation    As         =  ("As", 0.075)
pitchInformation    Af         =  ("Ab", 0.075)
pitchInformation    B          =  ("B", 0.1)
pitchInformation    Bs         =  ("Bs", 0.1)
pitchInformation    Bf         =  ("Bb", 0.1)
pitchInformation    _          =  ("unbekannt", (-0.1))

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

pitchToHeight :: PitchClass -> GLfloat
pitchToHeight    C          =  (-0.05)
pitchToHeight    D          =  (-0.025)
pitchToHeight    E          =  0
pitchToHeight    F          =  0.025
pitchToHeight    G          =  0.05
pitchToHeight    A          =  0.075
pitchToHeight    B          =  0.1
pitchToHeight    _          =  0
