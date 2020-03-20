{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.StateVar (($=))
import Data.Text (Text)
import qualified Data.Vector.Mutable as MBV (IOVector, new, read, write)
import Data.Word (Word8)
import Linear (V2(..), V4(..), (^+^))
import qualified SDL
import UIEvents (newElement, newElement_)
import qualified UIEvents (BubbleResult(..), CaptureResult(..),
                           DispatchResult(..), Location(..), MouseButton(..),
                           MouseButtonEvent(..), MouseButtonEventType(..),
                           MouseMotionEvent(..), UIElement(..), UIElementId,
                           UIEntity(..), UIEvent(..), UIEventPayload(..),
                           addUIElement, element, foldUIEntities,
                           modifyUIElement, newUIEventDispatcher, root,
                           setBubbleHandler, uieventDispatcherRoot)
import qualified UIEvents.SDL as UIEvents (pollEventsDispatch)

data Card = Card
    { cardPosition     :: !(V2 Double)
    , cardVelocity     :: !(V2 Double)
    , cardAcceleration :: !(V2 Double)
    } deriving (Show, Eq)

main :: IO ()
main = withSDL . withWindow "uievent-sdl2:example" (truncate windowWidth, truncate windowHeight) $ \w -> do
    renderer <- SDL.createRenderer w (-1) SDL.defaultRenderer
    dispatcher <- UIEvents.newUIEventDispatcher (V4 255 255 255 255 :: V4 Word8)
    mouseDownOn <- newIORef Nothing :: IO (IORef (Maybe UIEvents.UIElementId))
    let appRootElem = UIEvents.element (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 windowWidth windowHeight))
        d1 = UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 0 0) (V2 600 450))
        d2 = UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 600 0) (V2 600 450))
        d3 = UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 0 450) (V2 600 450))
        d4 = UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 600 450) (V2 600 450))
        e1 = UIEvents.element (V4 255 0 0 255) (UIEvents.Location (V2 20 20) (V2 180 180))
        e2 = UIEvents.element (V4 0 255 0 255) (UIEvents.Location (V2 210 20) (V2 180 180))
        e3 = UIEvents.element (V4 0 0 255 255) (UIEvents.Location (V2 400 20) (V2 180 180))
        e4 = UIEvents.element (V4 255 0 0 255) (UIEvents.Location (V2 10 10) (V2 75 75))
        e5 = UIEvents.element (V4 0 255 0 255) (UIEvents.Location (V2 95 10) (V2 75 75))
        appRootBubble _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonPressed UIEvents.MouseButtonLeft _))) target = do
            writeIORef mouseDownOn (Just target)
            return (UIEvents.Bubbled True Nothing)
        appRootBubble _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ = do
            writeIORef mouseDownOn Nothing
            return (UIEvents.Bubbled True Nothing)
        appRootBubble _ _ _ =
            return (UIEvents.Bubbled True Nothing)
        bubbleHandler entity (UIEvents.UIEvent _ payload @ (UIEvents.MouseButtonEvent' _)) _ = do
            putStrLn $ show entity ++ " " ++ show payload
            return (UIEvents.Bubbled True Nothing)
        bubbleHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
        dragHandler entity (UIEvents.UIEvent _ (UIEvents.MouseMotionEvent' (UIEvents.MouseMotionEvent _ movement [UIEvents.MouseButtonLeft]))) target
            | UIEvents.uientityId entity == target = do
                downOn <- readIORef mouseDownOn
                if downOn == Just target
                    then
                        let element = UIEvents.uientityElement entity
                            location = UIEvents.uielementLocation element
                            movedPosition = UIEvents.locationPosition location ^+^ fmap fromIntegral movement
                            movedLocation = location { UIEvents.locationPosition = movedPosition }
                            movedElement = element { UIEvents.uielementLocation = movedLocation }
                        in return (UIEvents.Bubbled True (Just movedElement))
                    else return (UIEvents.Bubbled True Nothing)
            | otherwise = return (UIEvents.Bubbled True Nothing)
        dragHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
    flip runReaderT dispatcher $
        UIEvents.root $
            newElement (appRootElem { UIEvents.uielementBubbleHandler = appRootBubble }) $ do
                newElement d1 { UIEvents.uielementBubbleHandler = bubbleHandler } $ do
                    newElement e1 { UIEvents.uielementBubbleHandler = bubbleHandler } $
                        newElement_ e4 { UIEvents.uielementBubbleHandler = bubbleHandler }
                    newElement_ e2 { UIEvents.uielementBubbleHandler = bubbleHandler }
                    newElement e3 { UIEvents.uielementBubbleHandler = bubbleHandler } $ do
                        (_, eid) <- ask
                        _ <- lift $ addModal dispatcher (createModal dispatcher eid)
                        return ()
                newElement d2 { UIEvents.uielementBubbleHandler = bubbleHandler } $ do
                    (_, eid) <- ask
                    _ <- lift $ addCards dispatcher eid 12
                    return ()
                newElement d3 { UIEvents.uielementBubbleHandler = bubbleHandler } $
                    newElement_ e5 { UIEvents.uielementBubbleHandler = dragHandler }
                newElement_ d4 { UIEvents.uielementBubbleHandler = bubbleHandler }
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
        SDL.delay 50
        return r

    eventLoop dispatcher renderer = do
        r <- tick dispatcher renderer
        case r of
            UIEvents.DispatchExit -> putStrLn "dispatch exit"
            _                     -> eventLoop dispatcher renderer

    createModal dispatcher startElemId modalRoot startModal endModal = do
        let startHandler _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ =
                startModal >> return (UIEvents.Bubbled True Nothing)
            startHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
            endHandler _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ =
                endModal >> return (UIEvents.Bubbled True Nothing)
            endHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
            modalElement = UIEvents.element (V4 200 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 100 200))
        UIEvents.setBubbleHandler dispatcher startElemId startHandler
        UIEvents.addUIElement dispatcher modalRoot modalElement { UIEvents.uielementBubbleHandler = endHandler }

    addModal dispatcher f = do
        let root = UIEvents.uieventDispatcherRoot dispatcher
            captureAll _ _ _ = return (UIEvents.Captured True)
            modalRootElement = (UIEvents.element (V4 0 0 0 32 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 windowWidth windowHeight)))
                { UIEvents.uielementDisplay = False
                , UIEvents.uielementZIndex = 1
                , UIEvents.uielementCaptureHandler = captureAll
                }
        modalRoot <- UIEvents.addUIElement dispatcher root modalRootElement
        let startModal = UIEvents.modifyUIElement dispatcher (`setUIElementDisplay` True) modalRoot
            endModal = UIEvents.modifyUIElement dispatcher (`setUIElementDisplay` False) modalRoot
        f modalRoot startModal endModal

    setUIElementDisplay a b = a { UIEvents.uielementDisplay = b }

    addCards dispatcher parent n = do
        store <- MBV.new n :: IO (MBV.IOVector Card)
        forM_ [0..(n-1)] $ \i -> do
            let x = fromIntegral $ 20 + i * 40
                y = 20
                card = Card (V2 x y) (V2 0 0) (V2 0 0)
            MBV.write store i card
        forM [0..(n-1)] $ \i -> do
            card <- MBV.read store i
            let pos = cardPosition card
                element = UIEvents.element (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location pos (V2 100 200))
            UIEvents.addUIElement dispatcher parent element
