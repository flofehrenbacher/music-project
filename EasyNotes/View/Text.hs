{-# LANGUAGE FlexibleContexts #-}
module View.Text where

import View.NoteLine

import Data.Text
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

haltingPosition :: GLfloat
haltingPosition = 0.05

yCoordC :: GLfloat
yCoordC =  1.2285

-- | reduces a constant (1.4) according to the passed time
-- when x becomes less than the specified haltingPosition it stays the same
placeNoteToBePlayed :: NominalDiffTime -> GLfloat
placeNoteToBePlayed difference = let x =  1.4 - (realToFrac (difference) / 4) in
    if x > haltingPosition then x else haltingPosition

-- | displays the note with its possible helping line on the note lines
displayNoteAboveKeyboard :: (String, GLfloat) -> GLfloat -> IO()
displayNoteAboveKeyboard    (note, height)   x = do
        translate$Vector3 (x + 0.02::GLfloat) 0 0
        if height < 0 then preservingMatrix helpingLine else return ()
        translate$Vector3 0 (yCoordC + height::GLfloat ) 0
        lineWidth $= 2
        currentColor $= Color4 1 1 1 1
        scale 0.0007 0.00065 (0::GLfloat)
        displayNote note
            where displayNote :: String -> IO()
                  displayNote    note | takeEnd 1 (pack note) == pack "s" = renderString Roman "#o"
                  displayNote    note | takeEnd 1 (pack note) == pack "f" = renderString Roman "bo"
                  displayNote    note | otherwise = renderString Roman "o"

displayTextAboveKeyboard :: String -> GLfloat -> IO()
displayTextAboveKeyboard    text x = do
        translate$Vector3 (x::GLfloat) 0.95 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 3
        scale 0.0011 0.0011 (0::GLfloat)
        renderString Roman text
