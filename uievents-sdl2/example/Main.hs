{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.StateVar (($=))
import Data.Text (Text)
import Linear (V2(..), V4(..))
import qualified SDL

main :: IO ()
main = withSDL . withWindow "uievent-sdl2:example" (640, 480) $ \w -> do
    renderer <- SDL.createRenderer w (-1) SDL.defaultRenderer
    draw renderer
    SDL.delay 10000

    where
    withSDL a = SDL.initialize [] >> a >> SDL.quit

    withWindow :: Text -> (Int, Int) -> (SDL.Window -> IO a) -> IO ()
    withWindow title (width, height) f = do
        window <- SDL.createWindow title
            SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height) }
        SDL.showWindow window
        _ <- f window
        SDL.destroyWindow window

    draw r = do
        SDL.clear r
        SDL.rendererDrawColor r $= V4 255 0 0 255
        SDL.fillRect r . Just $ SDL.Rectangle (SDL.P (V2 50 100)) (V2 200 100)
        SDL.present r
