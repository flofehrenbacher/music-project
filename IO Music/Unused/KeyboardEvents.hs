{-# LANGUAGE FlexibleContexts #-}
module KeyboardEvents where

import Euterpea

import Data.Char
import Text.Read
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import StateRecorder

keyboard recorder (Graphics.UI.GLUT.Char 'r') Down _ _ = do
    r <- Graphics.Rendering.OpenGL.get recorder
    playS $ line $ reverse (notes r)
    postRedisplay Nothing

keyboard recorder (Graphics.UI.GLUT.Char stroke) Down _ _ = do    
    case convertStrokeToPitchClass stroke of
        Just x -> do
            let noteToBePlayed = note 0.25 (x,4::Octave)
            play $ noteToBePlayed
            r <- Graphics.Rendering.OpenGL.get recorder
            recorder' <- recordIO r noteToBePlayed
            recorder $= recorder'
        Nothing -> return ()
    postRedisplay Nothing
keyboard _  _ _ _ _ = return ()

convertStrokeToPitchClass :: Char -> Maybe PitchClass
convertStrokeToPitchClass stroke = do
    let upper = toUpper stroke
    readMaybe [upper] :: Maybe PitchClass