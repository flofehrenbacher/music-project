{-# LANGUAGE FlexibleContexts #-}
module MusicWithKeys where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import Euterpea
import StateRecorder

-- für Interaktion
import Data.IORef
new = newIORef

main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 500 500
    createAWindow progName
    mainLoop

createAWindow windowName = do
    createWindow windowName
    recorder <- new $ MyRecorder []
    music <- new c
    displayCallback $= display
    keyboardMouseCallback $= Just (keyboard recorder)

display = do
    clear [ColorBuffer]
    loadIdentity
    swapBuffers
    flush

-- keystroke events
keyboard recorder (Char 'c') Down _ _ = do
    play $ c 4 qn
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
keyboard recorder (Char 'r') Down _ _ = do
    r <- get recorder
    play $ line $ reverse (notes r)
    postRedisplay Nothing
keyboard _  _ _ _ _ = return ()