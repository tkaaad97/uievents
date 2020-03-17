module UIEvents
    ( module UIEvents.Types
    , newUIEventDispatcher
    , addUIElement
    , removeUIElement
    , modifyUIElement
    , setZIndex
    , setCaptureHandler
    , setBubbleHandler
    , sliceUIEntities
    , foldUIEntities
    , dispatchUIEvent
    , defaultHandlers
    ) where

import Control.Exception (throwIO)
import Control.Monad (mplus, msum, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, maybe)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV (Vector, filter, foldM', imapM_, length, map,
                                    mapM, mapM_, snoc, take, unsafeFreeze)
import qualified Data.Vector.Algorithms.Intro as VA (sortBy)
import qualified Data.Vector.Mutable as MBV (IOVector, MVector, new, write)
import Linear (V2(..), V3(..), (!*))
import qualified UIEvents.Internal.Component as Component (ComponentStore,
                                                           addComponent,
                                                           modifyComponent,
                                                           newComponentStore,
                                                           readComponent,
                                                           removeComponent,
                                                           unsafeGetComponentVector)
import UIEvents.Types


updateUIEntityContent :: UIEntity a -> UIElement a -> UIEntity a
updateUIEntityContent entity element = modifyUIEntityContent (const element) entity

setUIEntityUpdated :: UIEntity a -> Bool -> UIEntity a
setUIEntityUpdated a b = a { uientityUpdated = b }

modifyUIEntityContent :: (UIElement a -> UIElement a) -> UIEntity a -> UIEntity a
modifyUIEntityContent f entity = entity { uientityContent = f . uientityContent $ entity }

newUIEventDispatcher :: UIElement a -> IO (UIEventDispatcher a)
newUIEventDispatcher rootElem = do
    let rootId = UIElementId 1
        rootEntity = UIEntity rootId mempty Nothing rootElem 0 mempty rootHandlers False
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a)))
    Component.addComponent store rootId rootEntity
    counter <- Counter.newCounter 1
    return (UIEventDispatcher counter rootId store)
    where
    rootHandlers = UIElementHandlers rootCapture rootBubble
    rootCapture _ (UIEvent _ (WindowResizeEvent' _)) = return (Captured False)
    rootCapture _ (UIEvent _ (WindowCloseEvent' _))  = return (Captured False)
    rootCapture _ _                                  = return (Captured True)
    rootBubble _ (UIEvent _ (WindowCloseEvent' _)) _ = return BubbledExit
    rootBubble _ _ _ = return (Bubbled False Nothing)

addUIElement :: UIEventDispatcher a -> UIElementId -> UIElement a -> UIElementHandlers a -> IO UIElementId
addUIElement dispatcher parent element handlers = do
    elemId <- UIElementId <$> Counter.incrCounter 1 counter
    let entity = UIEntity elemId mempty (Just parent) element 0 mempty handlers False
    Component.addComponent store elemId entity
    r <- Component.modifyComponent store (flip setUIEntityUpdated True . addChild elemId) parent
    unless r . throwIO . userError $ "element not found: " ++ show parent
    return elemId
    where
    counter = uieventDispatcherElementCounter dispatcher
    store = uieventDispatcherElements dispatcher
    addChild c e =
        e { uientityChildren = uientityChildren e `BV.snoc` c }

modifyUIElement :: UIEventDispatcher a -> (UIElement a -> UIElement a) -> UIElementId -> IO ()
modifyUIElement dispatcher f elemId = do
    r <- Component.modifyComponent store (modifyUIEntityContent f) elemId
    unless r . throwIO . userError $ "element not found: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher

readComponent :: Component.ComponentStore MBV.MVector UIElementId (UIEntity a) -> UIElementId -> IO (UIEntity a)
readComponent store eid = liftIO (maybe (throwIO . userError $ "element not found: " ++ show eid) return =<< Component.readComponent store eid)

setZIndex :: UIEventDispatcher a -> UIElementId -> Int -> IO ()
setZIndex dispatcher elemId z = do
    _ <- Component.modifyComponent store f elemId
    entity <- readComponent store elemId
    case uientityParent entity of
        Just parent -> void $ Component.modifyComponent store (`setUIEntityUpdated` True) parent
        Nothing -> return ()
    where
    store = uieventDispatcherElements dispatcher
    f a = a { uientityZIndex = z }

setCaptureHandler :: UIEventDispatcher a -> UIElementId -> CaptureHandler a -> IO ()
setCaptureHandler dispatcher elemId handler = do
    _ <- Component.modifyComponent store f elemId
    return ()
    where
    store = uieventDispatcherElements dispatcher
    f a = a { uientityHandlers = (uientityHandlers a) { captureHandler = handler } }

setBubbleHandler :: UIEventDispatcher a -> UIElementId -> BubbleHandler a -> IO ()
setBubbleHandler dispatcher elemId handler = do
    _ <- Component.modifyComponent store f elemId
    return ()
    where
    store = uieventDispatcherElements dispatcher
    f a = a { uientityHandlers = (uientityHandlers a) { bubbleHandler = handler } }

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
    root <- liftIO $ readComponent store rootId
    go x y root
    where
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherElements dispatcher
    go x0 y0 entity = do
        (x1, y1) <- f entity x0 y0
        let elemIds = uientityChildren entity
        entities <- liftIO $ BV.mapM (readComponent store) elemIds
        BV.foldM' (go x1) y1 entities

capturePhase :: UIEventDispatcher a -> UIEvent -> IO [UIEntity a]
capturePhase dispatcher event = do
    root <- readComponent store rootId
    fromMaybe [] <$> go [] root
    where
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherElements dispatcher

    go xs entity = do
        r <- f xs entity
        case r of
            Just (True, xs') -> do
                children <- getZSortedChildren dispatcher entity
                (`mplus` Just xs') . msum <$> BV.mapM (go xs') children
            Just (False, xs') -> return $ Just xs'
            Nothing -> return Nothing

    f xs entity
        | uielementDisplay . uientityContent $ entity = do
            let handler = captureHandler . uientityHandlers $ entity
            captureResult <- liftIO $ handler entity event
            case captureResult of
                Captured True  -> return . Just $ (True, entity : xs)
                Captured False -> return . Just $ (False, entity : xs)
                Uncaptured     -> return Nothing
        | otherwise = return Nothing

getZSortedChildren :: UIEventDispatcher a -> UIEntity a -> IO (BV.Vector (UIEntity a))
getZSortedChildren dispatcher entity
    | uientityUpdated entity = do
        entities <- MBV.new (BV.length children)
        BV.imapM_ (\i e -> readComponent store e >>= MBV.write entities i) children
        VA.sortBy compareZIndexDesc entities
        freezed <- BV.unsafeFreeze entities
        let zsortedChildren = BV.map uientityId freezed
        _ <- Component.modifyComponent store (`updateZSortedChildren` zsortedChildren) (uientityId entity)
        return freezed
    | otherwise =
        BV.mapM (readComponent store) (uientityZSortedChildren entity)
    where
    store = uieventDispatcherElements dispatcher
    children = uientityChildren entity
    compareZIndexDesc e1 e2 =
        compare (uientityZIndex e2) (uientityZIndex e1)
    updateZSortedChildren a b = a { uientityZSortedChildren = b, uientityUpdated = False }

bubblePhase :: UIEventDispatcher a -> [UIEntity a] -> UIEvent -> UIElementId -> IO DispatchResult
bubblePhase _ [] _ _ = return DispatchContinue
bubblePhase dispatcher (entity : es) event target = do
    bubbleResult <- handler entity event target
    case bubbleResult of
        Bubbled propagate Nothing ->
            if propagate
                then bubblePhase dispatcher es event target
                else return DispatchContinue
        Bubbled propagate (Just element) -> do
            _ <- Component.modifyComponent store (`updateUIEntityContent` element) (uientityId entity)
            if propagate
                then bubblePhase dispatcher es event target
                else return DispatchContinue
        BubbledExit ->
            return DispatchExit
    where
    handler = bubbleHandler . uientityHandlers $ entity
    store = uieventDispatcherElements dispatcher

dispatchUIEvent :: UIEventDispatcher a -> UIEvent -> IO DispatchResult
dispatchUIEvent dispatcher event = do
    es <- capturePhase dispatcher event
    let target = if null es
                    then uieventDispatcherRoot dispatcher
                    else uientityId . head $ es
    bubblePhase dispatcher es event target

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

    bh _ _ _ = return (Bubbled True Nothing)

insideLocation :: V2 Int32 -> Location -> Bool
insideLocation (V2 px py) (Location (V2 x y) (V2 width height) theta) =
    px' >= 0 && px' <= width && py' >= 0 && py' <= height
    where
    m = V2 (V3 (cos (-theta)) (- sin (-theta)) (-x))
           (V3 (sin (-theta)) (cos (-theta)) (-y))
    V2 px' py' = m !* V3 (fromIntegral px) (fromIntegral py) 1
