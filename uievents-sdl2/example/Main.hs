{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.StateVar (($=))
import Data.Text (Text)
import Data.Word (Word8)
import Linear (V2(..), V4(..), (^+^))
import qualified SDL
import qualified UIEvents (BubbleResult(..), DispatchResult(..), Location(..),
                           UIElement(..), UIElementHandlers(..), UIEntity(..),
                           UIEvent(..), UIEventPayload(..), addUIElement,
                           defaultHandlers, foldUIEntities,
                           newUIEventDispatcher, uieventDispatcherRoot)
import qualified UIEvents.SDL as UIEvents (pollEventDispatch)

main :: IO ()
main = withSDL . withWindow "uievent-sdl2:example" (640, 480) $ \w -> do
    renderer <- SDL.createRenderer w (-1) SDL.defaultRenderer
    dispatcher <- UIEvents.newUIEventDispatcher (UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 640 480) 0) True)
    let root = UIEvents.uieventDispatcherRoot dispatcher
        e1 = UIEvents.UIElement (V4 255 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 200 200) 0) True
        e2 = UIEvents.UIElement (V4 0 255 0 255 :: V4 Word8) (UIEvents.Location (V2 215 10) (V2 200 200) 0) True
        e3 = UIEvents.UIElement (V4 0 0 255 255 :: V4 Word8) (UIEvents.Location (V2 420 10) (V2 200 200) 0) True
        e4 = UIEvents.UIElement (V4 255 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 80 80) 0) True
        e5 = UIEvents.UIElement (V4 0 255 0 255 :: V4 Word8) (UIEvents.Location (V2 95 10) (V2 80 80) 0) True
        bubbleHandler entity (UIEvents.UIEvent _ payload @ (UIEvents.MouseButtonEvent' _)) = do
            putStrLn $ show entity ++ " " ++ show payload
            return (UIEvents.Bubbled True Nothing)
        bubbleHandler _ _ = return (UIEvents.Bubbled True Nothing)
        handlers = UIEvents.defaultHandlers { UIEvents.bubbleHandler = bubbleHandler }
    elemId1 <- UIEvents.addUIElement dispatcher root e1 handlers
    _ <- UIEvents.addUIElement dispatcher root e2 handlers
    _ <- UIEvents.addUIElement dispatcher root e3 handlers
    _ <- UIEvents.addUIElement dispatcher elemId1 e4 handlers
    _ <- UIEvents.addUIElement dispatcher elemId1 e5 handlers
    eventLoop dispatcher renderer

    where
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
        UIEvents.foldUIEntities d (drawEntity r) (V2 0 0) ()
        SDL.present r

    drawEntity r e p0 _ = liftIO $ do
        let element = UIEvents.uientityContent e
            color = UIEvents.uielementValue element
            UIEvents.Location p1 size _ = UIEvents.uielementLocation element
            position = fmap floor $ p0 ^+^ p1
            rect = SDL.Rectangle (SDL.P position) (fmap floor size)
        when (UIEvents.uielementDisplay element) $ do
            SDL.rendererDrawColor r $= color
            SDL.fillRect r (Just rect)
            SDL.rendererDrawColor r $= V4 64 64 64 255
            SDL.drawRect r (Just rect)
            SDL.rendererDrawColor r $= V4 0 0 0 255
        return (p1, ())

    tick dispatcher renderer = do
        threadDelay 50
        r <- UIEvents.pollEventDispatch dispatcher (const $ return UIEvents.DispatchContinue)
        _ <- runMaybeT $ draw renderer dispatcher
        return r

    eventLoop dispatcher renderer = do
        r <- tick dispatcher renderer
        case r of
            Just UIEvents.DispatchExit -> putStrLn "dispatch exit"
            _                          -> eventLoop dispatcher renderer
