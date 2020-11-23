{-# LANGUAGE OverloadedStrings #-}
module Example
    ( createUI
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Mutable as MBV (IOVector, new, read, write)
import Data.Word (Word8)
import Linear (V2(..), V4(..), (^+^))
import UIEvents (child, child_, setBubbleHandler)
import qualified UIEvents (BubbleResult(..), CaptureResult(..), Location(..),
                           MouseButton(..), MouseButtonEvent(..),
                           MouseButtonEventType(..), MouseMotionEvent(..),
                           UIElement(..), UIElementId, UIEntity(..),
                           UIEvent(..), UIEventDispatcher, UIEventPayload(..),
                           addUIElement, element, modifyUIElement,
                           newUIEventDispatcher, root,
                           setUIElementBubbleHandler, uieventDispatcherRoot)

data Card = Card
    { cardPosition     :: !(V2 Double)
    , cardVelocity     :: !(V2 Double)
    , cardAcceleration :: !(V2 Double)
    } deriving (Show, Eq)

createUI :: Double -> Double -> IO (UIEvents.UIEventDispatcher (V4 Word8))
createUI windowWidth windowHeight = do
    dispatcher <- UIEvents.newUIEventDispatcher (V4 255 255 255 255)
    mouseDownOn <- newIORef Nothing :: IO (IORef (Maybe UIEvents.UIElementId))
    let rootBubble _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonPressed UIEvents.MouseButtonLeft _))) target = do
            writeIORef mouseDownOn (Just target)
            return (UIEvents.Bubbled True Nothing)
        rootBubble _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ = do
            writeIORef mouseDownOn Nothing
            return (UIEvents.Bubbled True Nothing)
        rootBubble _ (UIEvents.UIEvent _ UIEvents.WindowLeaveEvent') _ = do
            writeIORef mouseDownOn Nothing
            return (UIEvents.Bubbled True Nothing)
        rootBubble _ (UIEvents.UIEvent _ (UIEvents.WindowCloseEvent' _)) _ = return UIEvents.BubbledExit
        rootBubble _ _ _ =
            return (UIEvents.Bubbled False Nothing)
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
        UIEvents.root $ do
            setBubbleHandler rootBubble
            let d1 = (UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 0 0) (V2 600 450))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                d2 = (UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 600 0) (V2 600 450))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                d3 = (UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 0 450) (V2 600 450))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                d4 = (UIEvents.element (V4 255 255 255 255) (UIEvents.Location (V2 600 450) (V2 600 450))) { UIEvents.uielementBubbleHandler = bubbleHandler }
            modalStartElemId <- child d1 $ do
                let e1 = (UIEvents.element (V4 255 0 0 255) (UIEvents.Location (V2 20 20) (V2 180 180))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                    e2 = (UIEvents.element (V4 0 255 0 255) (UIEvents.Location (V2 210 20) (V2 180 180))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                    e3 = (UIEvents.element (V4 0 0 255 255) (UIEvents.Location (V2 400 20) (V2 180 180))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                    e4 = (UIEvents.element (V4 255 0 0 255) (UIEvents.Location (V2 10 10) (V2 75 75))) { UIEvents.uielementBubbleHandler = bubbleHandler }
                child e1 $
                    child_ e4
                child_ e2
                child e3 $ fmap snd ask
            child d2 $ do
                (_, eid) <- ask
                _ <- lift $ addCards dispatcher eid 12
                return ()
            child d3 $ do
                let e5 = (UIEvents.element (V4 0 255 0 255) (UIEvents.Location (V2 95 10) (V2 75 75))) { UIEvents.uielementBubbleHandler = dragHandler }
                child_ e5
            child_ d4
            _ <- lift $ addModal dispatcher (createModal dispatcher modalStartElemId)
            return ()
    return dispatcher

    where
    createModal dispatcher startElemId modalRoot startModal endModal = do
        let startHandler _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ =
                startModal >> return (UIEvents.Bubbled True Nothing)
            startHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
            endHandler _ (UIEvents.UIEvent _ (UIEvents.MouseButtonEvent' (UIEvents.MouseButtonEvent UIEvents.MouseButtonReleased UIEvents.MouseButtonLeft _))) _ =
                endModal >> return (UIEvents.Bubbled True Nothing)
            endHandler _ _ _ = return (UIEvents.Bubbled True Nothing)
            modalElement = UIEvents.element (V4 200 0 0 255 :: V4 Word8) (UIEvents.Location (V2 10 10) (V2 100 200))
        UIEvents.setUIElementBubbleHandler dispatcher startElemId startHandler
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
