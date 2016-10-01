-- | This module defines functions dealing with PitchClasses
module PitchClass where

import Euterpea
import Graphics.UI.GLUT

-- | Checks whether two PitchClasses are the same.
--
--   If at least one argument is 'Nothing' returns 'False'
isAbsPitchTheSame :: Maybe PitchClass -> Maybe PitchClass                                   -> Bool
isAbsPitchTheSame   (Just  pcOne)       (Just pcTwo)       | pcToInt pcOne == pcToInt pcTwo = True
                                                           | otherwise                      = False
isAbsPitchTheSame   _                   _                                                   = False

-- | Returns a tuple of the name of the pitch class and the 
-- height where it has to be displayed on the note line. 
pitchInformation :: PitchClass -> (String, GLfloat)
pitchInformation    pitchClass =  (show pitchClass, heightOf pitchClass)
    where heightOf :: PitchClass -> GLfloat
          heightOf pitchClass = initialHeight + (0.025 * (nthMainNote (takeMainPitchClass pitchClass)))
               where initialHeight = -0.05

-- | Assigns a number according to the order of the white keys of a piano. 
--   Starting with /C:0/ until /B:6/
nthMainNote :: PitchClass -> GLfloat
nthMainNote    C          =  0
nthMainNote    D          =  1
nthMainNote    E          =  2
nthMainNote    F          =  3
nthMainNote    G          =  4
nthMainNote    A          =  5
nthMainNote    B          =  6

-- | Transforms a PitchClass into its main PitchClass
--
--   /Ef/ becomes /E/
takeMainPitchClass :: PitchClass -> PitchClass
takeMainPitchClass    pitchClass = read $ charToString $ Prelude.head $ show pitchClass
    where charToString :: Char -> String
          charToString = (:[])