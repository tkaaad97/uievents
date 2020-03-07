module UIEvents
    ( module UIEvents.Types
    , newUIEventDispatcher
    , addUIElement
    , removeUIElement
    , modifyUIElement
    , sliceUIEntities
    , foldUIEntities
    , dispatchUIEvent
    , defaultHandlers
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV (Vector, filter, foldM', mapM, mapM_, snoc,
                                    take)
import qualified Data.Vector.Mutable as MBV (IOVector)
import Linear (V2(..), V3(..), (!*))
import qualified UIEvents.Internal.Component as Component (addComponent,
                                                           modifyComponent,
                                                           newComponentStore,
                                                           readComponent,
                                                           removeComponent,
                                                           unsafeGetComponentVector)
import UIEvents.Types


updateUIEntityContent :: UIEntity a -> UIElement a -> UIEntity a
updateUIEntityContent entity element = modifyUIEntityContent (const element) entity

modifyUIEntityContent :: (UIElement a -> UIElement a) -> UIEntity a -> UIEntity a
modifyUIEntityContent f entity = entity { uientityContent = f . uientityContent $ entity }

newUIEventDispatcher :: UIElement a -> IO (UIEventDispatcher a)
newUIEventDispatcher rootElem = do
    let rootId = UIElementId 1
        rootEntity = UIEntity rootId mempty Nothing rootElem rootHandlers
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a)))
    Component.addComponent store rootId rootEntity
    counter <- Counter.newCounter 1
    return (UIEventDispatcher counter rootId store)
    where
    rootHandlers = UIElementHandlers rootCapture rootBubble
    rootCapture _ _ = return (Captured True)
    rootBubble _ (UIEvent _ (WindowCloseEvent' _)) = return BubbledExit
    rootBubble _ _ = return (Bubbled False Nothing)

addUIElement :: UIEventDispatcher a -> UIElementId -> UIElement a -> UIElementHandlers a -> IO UIElementId
addUIElement dispatcher parent element handlers = do
    elemId <- UIElementId <$> Counter.incrCounter 1 counter
    let entity = UIEntity elemId mempty (Just parent) element handlers
    Component.addComponent store elemId entity
    r <- Component.modifyComponent store (addChild elemId) parent
    unless r . throwIO . userError $ "element not found: " ++ show parent
    return elemId
    where
    counter = uieventDispatcherElementCounter dispatcher
    store = uieventDispatcherElements dispatcher
    addChild c e =
        e { uientityChildren = uientityChildren e `BV.snoc` c }

modifyUIElement :: UIEventDispatcher a -> (UIElement a -> UIElement a) -> UIElementId -> IO Bool
modifyUIElement dispatcher f =
    Component.modifyComponent store (modifyUIEntityContent f)
    where
    store = uieventDispatcherElements dispatcher

removeUIElement :: UIEventDispatcher a -> UIElementId -> IO ()
removeUIElement dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    BV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
    parentModified <- maybe (return False) (Component.modifyComponent store removeChild) $ uientityParent e
    unless parentModified . throwIO . userError $ "parent element not found"
    deleted <- Component.removeComponent store elemId
    unless deleted . throwIO . userError $ "delete failed: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher
    removeChild e =
        e { uientityChildren = BV.filter (/= elemId) $ uientityChildren e }

removeUIElement_ :: UIEventDispatcher a -> UIElementId -> IO ()
removeUIElement_ dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    BV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
    deleted <- Component.removeComponent store elemId
    unless deleted . throwIO . userError $ "delete failed: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher

sliceUIEntities :: UIEventDispatcher a -> IO (BV.Vector (UIEntity a))
sliceUIEntities dispatcher = do
    (v, n) <- Component.unsafeGetComponentVector store
    return $ BV.take n v
    where
    store = uieventDispatcherElements dispatcher

foldUIEntities :: MonadIO m => UIEventDispatcher a -> (UIEntity a -> x -> y -> m (x, y)) -> x -> y -> m y
foldUIEntities dispatcher f x y = do
    root <- readComponent rootId
    go x y root
    where
    readComponent eid = liftIO (maybe (throwIO . userError $ "element not found: " ++ show eid) return =<< Component.readComponent store eid)
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherElements dispatcher
    go x0 y0 entity = do
        (x1, y1) <- f entity x0 y0
        let elemIds = uientityChildren entity
        entities <- BV.mapM readComponent elemIds
        BV.foldM' (go x1) y1 entities

capturePhase :: UIEventDispatcher a -> UIEvent -> IO [UIEntity a]
capturePhase dispatcher event = foldUIEntities dispatcher f True []
    where
    f entity True xs = do
        let handler = captureHandler . uientityHandlers $ entity
        captureResult <- liftIO $ handler entity event
        case captureResult of
            Captured True  -> return (True, entity : xs)
            Captured False -> return (False, entity : xs)
            Uncaptured     -> return (False, xs)
    f _ False xs = return (False, xs)

bubblePhase :: UIEventDispatcher a -> [UIEntity a] -> UIEvent -> IO DispatchResult
bubblePhase _ [] _ = return DispatchContinue
bubblePhase dispatcher (entity : es) event = do
    bubbleResult <- handler entity event
    case bubbleResult of
        Bubbled propagate Nothing ->
            if propagate
                then bubblePhase dispatcher es event
                else return DispatchContinue
        Bubbled propagate (Just element) -> do
            _ <- Component.modifyComponent store (`updateUIEntityContent` element) (uientityId entity)
            if propagate
                then bubblePhase dispatcher es event
                else return DispatchContinue
        BubbledExit ->
            return DispatchExit
    where
    handler = bubbleHandler . uientityHandlers $ entity
    store = uieventDispatcherElements dispatcher

dispatchUIEvent :: UIEventDispatcher a -> UIEvent -> IO DispatchResult
dispatchUIEvent dispatcher event = do
    es <- capturePhase dispatcher event
    bubblePhase dispatcher es event

defaultHandlers :: UIElementHandlers a
defaultHandlers = UIElementHandlers ch bh
    where
    ch _ (UIEvent _ (WindowResizeEvent' _))  = return (Captured True)
    ch _ (UIEvent _ (WindowCloseEvent' _))  = return (Captured True)
    ch e (UIEvent _ (MouseMotionEvent' ev))
        | insideLocation (mouseMotionEventPosition ev) (uielementLocation . uientityContent $ e) = return (Captured True)
        | otherwise = return Uncaptured
    ch e (UIEvent _ (MouseButtonEvent' ev))
        | insideLocation (mouseButtonEventPosition ev) (uielementLocation . uientityContent $ e) = return (Captured True)
        | otherwise = return Uncaptured
    ch _ (UIEvent _ (KeyboardEvent' _))     = return Uncaptured

    bh _ _ = return (Bubbled True Nothing)

insideLocation :: V2 Int32 -> Location -> Bool
insideLocation (V2 px py) (Location (V2 x y) (V2 width height) theta) =
    px' >= 0 && px' <= width && py' >= 0 && py' <= height
    where
    m = V2 (V3 (cos (-theta)) (- sin (-theta)) (-x))
           (V3 (sin (-theta)) (cos (-theta)) (-y))
    V2 px' py' = m !* V3 (fromIntegral px) (fromIntegral py) 1
