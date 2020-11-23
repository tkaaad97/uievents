{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.StateVar (($=))
import Data.Text (Text)
import Example (createUI)
import Linear (V2(..), V4(..), (^+^))
import qualified SDL
import qualified UIEvents (DispatchResult(..), Location(..), UIElement(..),
                           UIEntity(..), foldUIEntities)
import qualified UIEvents.SDL as UIEvents (pollEventsDispatch)

main :: IO ()
main = withSDL . withWindow "uievent-sdl2:example" (truncate windowWidth, truncate windowHeight) $ \w -> do
    renderer <- SDL.createRenderer w (-1) SDL.defaultRenderer
    dispatcher <- createUI windowWidth windowHeight
    eventLoop dispatcher renderer

    where
    windowWidth = 1200
    windowHeight = 900
    withSDL a = SDL.initialize [] >> a >> SDL.quit

    withWindow :: Text -> (Int, Int) -> (SDL.Window -> IO a) -> IO ()
    withWindow title (width, height) f = do
        window <- SDL.createWindow title
            SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height) }
        SDL.showWindow window
        _ <- f window
        SDL.destroyWindow window

    draw r d = do
        SDL.rendererDrawColor r $= V4 0 0 0 255
        SDL.clear r
        UIEvents.foldUIEntities d (drawEntity r) (True, V2 0 0) ()
        SDL.present r

    drawEntity r e (True, p0) _ = liftIO $ do
        let element = UIEvents.uientityElement e
            color = UIEvents.uielementValue element
            UIEvents.Location p1 size = UIEvents.uielementLocation element
            position = fmap floor $ p0 ^+^ p1
            rect = SDL.Rectangle (SDL.P position) (fmap floor size)
        when (UIEvents.uielementDisplay element) $ do
            SDL.rendererDrawColor r $= color
            SDL.fillRect r (Just rect)
            SDL.rendererDrawColor r $= V4 64 64 64 255
            SDL.drawRect r (Just rect)
            SDL.rendererDrawColor r $= V4 0 0 0 255
        return ((UIEvents.uielementDisplay element, p1), ())

    drawEntity _ _ (False, p1) _ = return ((False, p1), ())

    tick dispatcher renderer = do
        r <- UIEvents.pollEventsDispatch dispatcher (const $ return UIEvents.DispatchContinue) id
        _ <- draw renderer dispatcher
        SDL.delay 20
        return r

    eventLoop dispatcher renderer = do
        r <- tick dispatcher renderer
        case r of
            UIEvents.DispatchExit -> putStrLn "dispatch exit"
            _                     -> eventLoop dispatcher renderer
