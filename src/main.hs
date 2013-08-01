
module Main where

import Control.Monad (liftM, unless, when)
import Control.Monad.Extra
import Control.Applicative ((<$>), pure)
import Data.Monoid
import Data.Maybe

import qualified Graphics.Rendering.GLU.Raw as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core32 
import qualified Graphics.UI.GLFW as GLFW


import System.IO

import Control.Error

import Processing.Shader

{-}
showMonitor :: GLFW.Monitor -> IO String
showMonitor m = do
    (x, y) <- GLFW.getMonitorPhysicalSize m
    pure $ show x <> " x " <> show y



main :: IO ()
main = do
    r <- GLFW.init
    when r $ do
        monitors <- GLFW.getMonitors
        putStrLn =<< (liftM unlines $ case monitors of 
            Nothing -> pure ["No monitors"]
            Just mons -> mapM showMonitor mons)

        --putStrLn $ unlines x
-}
--putStrLn <$> ((liftM unlines) $

errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr (show err <> description) 

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
    GLFW.setWindowShouldClose window True
  
main :: IO ()
main = do
    GLFW.setErrorCallback $ Just errorCallback
    r <- GLFW.init
    when r $ do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        window <- GLFW.createWindow 800 600 "Processing" Nothing Nothing
        case window of 
            Just win -> do 
                GLFW.makeContextCurrent $ Just win
                GLFW.setKeyCallback win (Just keyCallback)
                GLFW.setWindowSizeCallback win $ Just windowSizeCallback

                glClearColor 0.05 0.05 0.05 1.0
                glDepthFunc gl_LESS


                x <- runEitherT createShaders
                case x of
                    Left e -> putStrLn $ unlines (map show e)
                    Right (x,y) -> putStrLn (show x ++ " " ++ show y)

    
                mainLoop win
                GLFW.destroyWindow win
            Nothing -> putStrLn "No window could be created."
        GLFW.terminate

mainLoop :: GLFW.Window -> IO ()
mainLoop w = om unless (GLFW.windowShouldClose w) $ do
    
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity
    GL.clearColor GL.$= GL.Color4 1.0 0.05 0.05 1
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop w


windowSizeCallback :: GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback _ w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GLU.gluPerspective 45 (fromIntegral w / fromIntegral h) 0.1 100

