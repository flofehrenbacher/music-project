module PitchClass where

import Euterpea
import Graphics.UI.GLUT


isAbsPitchTheSame :: Maybe PitchClass -> Maybe PitchClass                                   -> Bool
isAbsPitchTheSame   (Just  pcOne)       (Just pcTwo)       | pcToInt pcOne == pcToInt pcTwo = True
                                                           | otherwise                      = False
isAbsPitchTheSame   _                   _                                                   = False

pitchInformation :: PitchClass -> (String, GLfloat)
pitchInformation    pitchClass =  (show pitchClass, heightOf pitchClass)


heightOf :: PitchClass -> GLfloat
heightOf pitchClass = initialHeight + (0.025 * (heightMainNotes (takeMainPitchClass pitchClass)))
    where initialHeight = -0.05

heightMainNotes :: PitchClass -> GLfloat
heightMainNotes    C          =  0
heightMainNotes    D          =  1
heightMainNotes    E          =  2
heightMainNotes    F          =  3
heightMainNotes    G          =  4
heightMainNotes    A          =  5
heightMainNotes    B          =  6

takeMainPitchClass :: PitchClass -> PitchClass
takeMainPitchClass    pitchClass = read $ charToString $ Prelude.head $ show pitchClass

charToString :: Char -> String
charToString = (:[])