{-# LANGUAGE FlexibleContexts #-}
-- | This module contains functions concerning text which has to be displayed on the canvas
module View.Text where

import View.NoteLine

import Data.Text
import Data.Time.Clock
import Euterpea
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

-- | The 'haltingPosition' specifies at which x-coordinate the note has to stop moving
haltingPosition :: GLfloat
haltingPosition = 0.05

-- | 'yCoordC' is the y-coordinate where the note C has to be displayed
yCoordC :: GLfloat
yCoordC =  1.2285

-- | At this x-coordinate the note starts moving from the right to the halting position.
initNoteXCoord = 1.4

-- | Reduces the initNoteXCoord according to the passed time.
--
-- When it becomes less than the specified haltingPosition it stays the same.
placeNoteToBePlayed :: NominalDiffTime -> GLfloat
placeNoteToBePlayed    difference      = let xCoord =  1.4 - (realToFrac (difference) / 4) in
    if xCoord > haltingPosition then xCoord else haltingPosition

-- | Displays the note with its possible helping line on the note lines.
displayNoteAboveKeyboard :: (String, GLfloat) -> GLfloat -> IO()
displayNoteAboveKeyboard    (note, height)   xCoord = do
        translate$Vector3 (xCoord + 0.02::GLfloat) 0 0
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

-- | Displays the specified text at the specified x-coordinate
displayNoteName :: String -> GLfloat -> IO()
displayNoteName    noteName  xCoord  = do
        translate$Vector3 (xCoord::GLfloat) 0.95 0
        currentColor $= Color4 1 1 1 1
        lineWidth $= 3
        scale 0.0011 0.0011 (0::GLfloat)
        renderString Roman noteName
