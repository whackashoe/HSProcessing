
module Main where

import Control.Monad (liftM, unless, when)

import qualified Graphics.Rendering.GLU.Raw as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core32 
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
    _ <- GLFW.initialize
    _ <- GLFW.openWindow GLFW.defaultDisplayOptions 
        {   GLFW.displayOptions_numRedBits   = 8
        ,   GLFW.displayOptions_numGreenBits = 8
        ,   GLFW.displayOptions_numBlueBits  = 8
        ,   GLFW.displayOptions_numDepthBits = 1
        ,   GLFW.displayOptions_openGLForwardCompatible = True
        ,   GLFW.displayOptions_openGLVersion = (3, 2)
        ,   GLFW.displayOptions_openGLProfile = GLFW.CoreProfile
        }
        
    ver <- GLFW.getGlVersion     
    putStrLn $ "OpenGL Version: " ++ (show ver)
    
    GLFW.setWindowSizeCallback windowSizeCallback

    glClearColor 0.05 0.05 0.05 1.0
    glDepthFunc gl_LESS
  --  GL.clearColor    GL.$= GL.Color4 0.05 0.05 0.05 1
--    GL.depthFunc     GL.$= Just GL.Less
-- not in 3.2
--    GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
--    GL.shadeModel    GL.$= GL.Smooth

    
--    GL.lighting              GL.$= GL.Enabled
--    GL.lightModelAmbient     GL.$= GL.Color4 0.2 0.2 0.2 1
  --  GL.position (GL.Light 0) GL.$= GL.Vertex4 (-10) 10 (-10) 0
--    GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.4 0.4 0.4 1
--    GL.diffuse  (GL.Light 0) GL.$= GL.Color4 0.8 0.8 0.8 1
--    GL.light    (GL.Light 0) GL.$= GL.Enabled
    
    loop
    GLFW.closeWindow
    GLFW.terminate
    
    where
        loop = do
            GLFW.resetTime
            q0 <- GLFW.keyIsPressed GLFW.KeyEsc
            unless q0 $ do
                glClear (gl_DEPTH 
                GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                GL.loadIdentity
                GL.clearColor    GL.$= GL.Color4 1.0 0.05 0.05 1
                GLFW.swapBuffers
                loop 
                
    
            
            
        
windowSizeCallback :: Int -> Int -> IO ()
windowSizeCallback w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GLU.gluPerspective 45 (fromIntegral w / fromIntegral h) 0.1 100