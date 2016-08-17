module View.Clef where

import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Control.Monad

drawClef :: TextureObject -> IO ()
drawClef    clef          = do
    textureBinding Texture2D $= Just clef
    textureFilter Texture2D  $= ((Linear', Nothing), Linear')
    textureFunction          $= Replace
    renderPrimitive Quads $ do
        forM_ [(0,0),(0,1),(1,1),(1,0)] $ \(x,y) -> do
            texCoord $ TexCoord2 x y
            vertex $ Vertex3 x y (0 :: GLfloat)

loadClef :: IO TextureObject
loadClef = do
    eclef <- readTexture "EasyNotes/View/Clef.png"
    case eclef of
        Left message -> error $ "Clef.png does not exist. Message: " ++ message
        Right clef -> return clef