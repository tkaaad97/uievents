module Main where

import Control.Monad (when)
import Example (createUI)
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import System.IO.Error (userError)

main :: IO ()
main = withWindow "uievent-GLFW-b:example" (truncate windowWidth, truncate windowHeight) $ \w -> do
    _ <- createUI windowWidth windowHeight
    return ()
    where
    windowWidth = 1200
    windowHeight = 800

withWindow :: String -> (Int, Int) -> (GLFW.Window -> IO ()) -> IO ()
withWindow title (width, height) f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 5
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 32)
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setWindowSizeCallback win (Just resizeWindow)
              GLFW.setWindowCloseCallback win (Just shutdown)
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
    GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
