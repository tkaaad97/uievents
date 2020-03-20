{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.StateVar (($=))
import Data.Text (Text)
import qualified Data.Vector.Mutable as MBV (IOVector, new, read, write)
import Data.Word (Word8)
import Linear (V2(..), V4(..), (^+^))
import qualified SDL
import qualified UIEvents (BubbleResult(..), CaptureResult(..),
                           DispatchResult(..), Location(..), MouseButton(..),
                           MouseButtonEvent(..), MouseButtonEventType(..),
                           MouseMotionEvent(..), UIElement(..),
                           UIElementHandlers(..), UIElementId, UIEntity(..),
                           UIEvent(..), UIEventPayload(..), addUIElement,
                           defaultHandlers, foldUIEntities, modifyUIElement,
                           newUIEventDispatcher, setBubbleHandler, setZIndex,
                           uieventDispatcherRoot)
import qualified UIEvents.SDL as UIEvents (pollEventsDispatch)

data Card = Card
    { cardPosition     :: !(V2 Double)
    , cardVelocity     :: !(V2 Double)
    , cardAcceleration :: !(V2 Double)
    } deriving (Show, Eq)

main :: IO ()
main = withSDL . withWindow "uievent-sdl2:example" (truncate windowWidth, truncate windowHeight) $ \w -> do
    renderer <- SDL.createRenderer w (-1) SDL.defaultRenderer
    dispatcher <- UIEvents.newUIEventDispatcher (UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 windowWidth windowHeight)) True)
    mouseDownOn <- newIORef Nothing :: IO (IORef (Maybe UIEvents.UIElementId))
    let root = UIEvents.uieventDispatcherRoot dispatcher
        appRootElem = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 windowWidth windowHeight)) True
        d1 = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 600 450)) True
        d2 = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 600 0) (V2 600 450)) True
        d3 = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 0 450) (V2 600 450)) True
        d4 = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location (V2 600 450) (V2 600 450)) True
        e1 = UIEvents.UIElement (V4 255 0 0 255 :: V4 Word8) (UIEvents.Location (V2 20 20) (V2 180 180)) True
        e2 = UIEvents.UIElement (V4 0 255 0 255 :: V4 Word8) (UIEvents.Location (V2 210 20) (V2 180 180)) True
        e3 = UIEvents.UIElement (V4 0 0 255 255 :: V4 Word8) (UIEvents.Location (V2 400 20) (V2 180 180)) True
        e4 = UIEvents.UIElement (V4 255 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 75 75)) True
        e5 = UIEvents.UIElement (V4 0 255 0 255 :: V4 Word8) (UIEvents.Location (V2 95 10) (V2 75 75)) True
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
                        let element = UIEvents.uientityContent entity
                            location = UIEvents.uielementLocation element
                            movedPosition = UIEvents.locationPosition location ^+^ fmap fromIntegral movement
                            movedLocation = location { UIEvents.locationPosition = movedPosition }
                            movedElement = element { UIEvents.uielementLocation = movedLocation }
                        in return (UIEvents.Bubbled True (Just movedElement))
                    else return (UIEvents.Bubbled True Nothing)
            | otherwise = return (UIEvents.Bubbled True Nothing)
        dragHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
        handlers = UIEvents.defaultHandlers { UIEvents.bubbleHandler = bubbleHandler }
    appRoot <- UIEvents.addUIElement dispatcher root appRootElem UIEvents.defaultHandlers { UIEvents.bubbleHandler = appRootBubble }
    div1 <- UIEvents.addUIElement dispatcher appRoot d1 handlers
    div2 <- UIEvents.addUIElement dispatcher appRoot d2 handlers
    div3 <- UIEvents.addUIElement dispatcher appRoot d3 handlers
    _ <- UIEvents.addUIElement dispatcher appRoot d4 handlers
    elemId1 <- UIEvents.addUIElement dispatcher div1 e1 handlers
    _ <- UIEvents.addUIElement dispatcher div1 e2 handlers
    elemId3 <- UIEvents.addUIElement dispatcher div1 e3 handlers
    _ <- UIEvents.addUIElement dispatcher elemId1 e4 handlers
    _ <- UIEvents.addUIElement dispatcher div3 e5 UIEvents.defaultHandlers { UIEvents.bubbleHandler = dragHandler }
    _ <- addModal dispatcher (createModal dispatcher elemId3)
    _ <- addCards dispatcher div2 12
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
        let element = UIEvents.uientityContent e
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
            modalElement = UIEvents.UIElement (V4 200 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 100 200)) True
        UIEvents.setBubbleHandler dispatcher startElemId startHandler
        UIEvents.addUIElement dispatcher modalRoot modalElement UIEvents.defaultHandlers { UIEvents.bubbleHandler = endHandler }

    addModal dispatcher f = do
        let root = UIEvents.uieventDispatcherRoot dispatcher
            modalRootElement = UIEvents.UIElement (V4 0 0 0 32 :: V4 Word8) (UIEvents.Location (V2 0 0) (V2 windowWidth windowHeight)) False
            captureAll _ _ _ = return (UIEvents.Captured True)
            modalRootHandlers = UIEvents.defaultHandlers { UIEvents.captureHandler = captureAll }
        modalRoot <- UIEvents.addUIElement dispatcher root modalRootElement modalRootHandlers
        UIEvents.setZIndex dispatcher modalRoot 1
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
                element = UIEvents.UIElement (V4 255 255 255 255 :: V4 Word8) (UIEvents.Location pos (V2 100 200)) True
            UIEvents.addUIElement dispatcher parent element UIEvents.defaultHandlers
