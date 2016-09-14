{-# LANGUAGE FlexibleContexts #-}
module View.Text where

import View.NoteLine

import Data.Text
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

type XCoordinate = GLfloat

haltingPosition :: XCoordinate
haltingPosition = 0.05

placeNoteToBePlayed :: NominalDiffTime -> XCoordinate
placeNoteToBePlayed difference = let x =  1.4 - (realToFrac (difference) / 4) in
    if x > haltingPosition then x else haltingPosition

showNoteAboveKeyboard :: (String,GLfloat) -> XCoordinate -> IO()
showNoteAboveKeyboard    (noteName,height)   x = do
        translate$Vector3 (x + 0.02::GLfloat) 0 0
        if height < 0 then preservingMatrix helpingLine else return ()
        translate$Vector3 0 (1.2285 + height::GLfloat ) 0
        lineWidth $= 2
        currentColor $= Color4 1 1 1 1
        scale 0.0007 0.00065 (0::GLfloat)
        showNote noteName

showTextAboveKeyboard :: String -> XCoordinate -> IO()
showTextAboveKeyboard    text x = do
        translate$Vector3 (x::GLfloat) 0.95 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 3
        scale 0.0011 0.0011 (0::GLfloat)
        renderString Roman text

labelKeys :: IO ()
labelKeys = do
    currentColor $= Color4 0 0 0 1
    lineWidth $= 3
    scale 0.001 0.001 (0::GLfloat)
    translate$Vector3 (50::GLfloat) 20 0
    renderString Roman "C"
    nextLabel "D"
    nextLabel "E"
    nextLabel "F"
    nextLabel "G"
    nextLabel "A"
    nextLabel "B"

nextLabel :: String -> IO ()
nextLabel    label  = do
    translate$Vector3 (115::GLfloat) 0 0
    renderString Roman label

showNote :: String -> IO()
showNote    noteName | takeEnd 1 (pack noteName) == pack "s" = renderString Roman "#o"
showNote    noteName | takeEnd 1 (pack noteName) == pack "b" = renderString Roman "bo"
showNote    noteName | otherwise = renderString Roman "o"

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
