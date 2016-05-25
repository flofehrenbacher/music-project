{-# LANGUAGE FlexibleContexts #-}
module KeyboardEvents where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import StateRecorder
import Euterpea
import Data.IORef

keyboard recorder (Char 'c') Down _ _ = do
    play (c 4 qn)
    r <- get recorder
    recorder' <- recordIO r (c 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'd') Down _ _ = do
    play $ d 4 qn
    r <- get recorder
    recorder' <- recordIO r (d 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'e') Down _ _ = do
    play $ e 4 qn
    r <- get recorder
    recorder' <- recordIO r (e 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'f') Down _ _ = do
    play $ f 4 qn
    r <- get recorder
    recorder' <- recordIO r (f 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'g') Down _ _ = do
    play $ g 4 qn
    r <- get recorder
    recorder' <- recordIO r (g 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'a') Down _ _ = do
    play $ a 4 qn
    r <- get recorder
    recorder' <- recordIO r (a 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
keyboard recorder (Char 'b') Down _ _ = do
    play $ b 4 qn
    r <- get recorder
    recorder' <- recordIO r (b 4 qn)
    recorder $= recorder'
    postRedisplay Nothing
-- all notes that have been played until this function is called are played together
keyboard recorder (Char 'r') Down _ _ = do
    r <- get recorder
    play $ line $ reverse (notes r)
    postRedisplay Nothing
keyboard _  _ _ _ _ = return ()