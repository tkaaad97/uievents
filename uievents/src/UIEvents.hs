{-# LANGUAGE FlexibleContexts #-}
module UIEvents
    ( module UIEvents.Types
    , newUIEventDispatcher
    , element
    , root
    , newElement
    , newElement_
    , addUIElement
    , removeUIElement
    , modifyUIElement
    , setZIndex
    , setCaptureHandler
    , setBubbleHandler
    , sliceUIEntities
    , foldUIEntities
    , dispatchUIEvent
    ) where

import Control.Exception (throwIO)
import Control.Monad (mplus, msum, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, reader, runReaderT,
                                   withReaderT)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, maybe)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV (Vector, filter, foldM', imapM_, length, map,
                                    mapM, mapM_, snoc, take, unsafeFreeze)
import qualified Data.Vector.Algorithms.Intro as VA (sortBy)
import qualified Data.Vector.Mutable as MBV (IOVector, MVector, new, write)
import Linear (V2(..), (^+^))
import qualified UIEvents.Internal.Component as Component (ComponentStore,
                                                           addComponent,
                                                           modifyComponent,
                                                           newComponentStore,
                                                           readComponent,
                                                           removeComponent,
                                                           unsafeGetComponentVector)
import UIEvents.Types


updateUIEntityContent :: UIEntity a -> UIElement a -> UIEntity a
updateUIEntityContent entity el = modifyUIEntityElement (const el) entity

setUIEntityUpdated :: UIEntity a -> Bool -> UIEntity a
setUIEntityUpdated a b = a { uientityUpdated = b }

modifyUIEntityElement :: (UIElement a -> UIElement a) -> UIEntity a -> UIEntity a
modifyUIEntityElement f entity = entity { uientityElement = f . uientityElement $ entity }

newUIEventDispatcher :: a -> IO (UIEventDispatcher a)
newUIEventDispatcher rootValue = do
    let rootId = UIElementId 1
        rootElem = UIElement rootValue (Location (V2 0 0) (V2 0 0)) True 0 rootCapture rootBubble
        rootEntity = UIEntity rootId mempty Nothing rootElem mempty False
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a)))
    Component.addComponent store rootId rootEntity
    counter <- Counter.newCounter 1
    return (UIEventDispatcher counter rootId store)
    where
    rootCapture _ (UIEvent _ (WindowResizeEvent' _)) _ = return (Captured False)
    rootCapture _ (UIEvent _ (WindowCloseEvent' _)) _  = return (Captured False)
    rootCapture _ _ _                                  = return (Captured True)
    rootBubble _ (UIEvent _ (WindowCloseEvent' _)) _ = return BubbledExit
    rootBubble _ _ _ = return (Bubbled False Nothing)

addUIElement :: UIEventDispatcher a -> UIElementId -> UIElement a -> IO UIElementId
addUIElement dispatcher parent el = do
    elemId <- UIElementId <$> Counter.incrCounter 1 counter
    let entity = UIEntity elemId mempty (Just parent) el mempty False
    Component.addComponent store elemId entity
    r <- Component.modifyComponent store (flip setUIEntityUpdated True . addChild elemId) parent
    unless r . throwIO . userError $ "element not found: " ++ show parent
    return elemId
    where
    counter = uieventDispatcherElementCounter dispatcher
    store = uieventDispatcherElements dispatcher
    addChild c e =
        e { uientityChildren = uientityChildren e `BV.snoc` c }

element :: a -> Location -> UIElement a
element value location = e
    where
    e = UIElement value location True 0 ch bh
    ch _ (UIEvent _ (WindowResizeEvent' _)) _ = return (Captured True)
    ch _ (UIEvent _ (WindowCloseEvent' _)) _  = return (Captured True)
    ch entity (UIEvent _ (MouseMotionEvent' ev)) p0
        | insideLocation (mouseMotionEventPosition ev) (uielementLocation . uientityElement $ entity) p0 = return (Captured True)
        | otherwise = return Uncaptured
    ch entity (UIEvent _ (MouseButtonEvent' ev)) p0
        | insideLocation (mouseButtonEventPosition ev) (uielementLocation . uientityElement $ entity) p0 = return (Captured True)
        | otherwise = return Uncaptured
    ch _ (UIEvent _ (KeyboardEvent' _)) _     = return Uncaptured

    bh _ _ _ = return (Bubbled True Nothing)

root :: ReaderT (UIEventDispatcher a, UIElementId) IO b -> ReaderT (UIEventDispatcher a) IO b
root block = do
    rid <- reader uieventDispatcherRoot
    withReaderT (flip (,) rid) block

newElement_ :: UIElement a -> ReaderT (UIEventDispatcher a, UIElementId) IO ()
newElement_ el = do
    (dispatcher, pid) <- ask
    _ <- liftIO $ addUIElement dispatcher pid el
    return ()

newElement :: UIElement a -> ReaderT (UIEventDispatcher a, UIElementId) IO b -> ReaderT (UIEventDispatcher a, UIElementId) IO b
newElement el block = do
    (dispatcher, pid) <- ask
    eid <- liftIO $ addUIElement dispatcher pid el
    liftIO $ runReaderT block (dispatcher, eid)

modifyUIElement :: UIEventDispatcher a -> (UIElement a -> UIElement a) -> UIElementId -> IO ()
modifyUIElement = modifyUIElement' True

modifyUIElement' :: Bool -> UIEventDispatcher a -> (UIElement a -> UIElement a) -> UIElementId -> IO ()
modifyUIElement' True dispatcher f elemId = do
    before <- readComponent store elemId
    r <- Component.modifyComponent store (modifyUIEntityElement f) elemId
    unless r . throwIO . userError $ "element not found: " ++ show elemId
    after <- readComponent store elemId
    let beforeZ = uielementZIndex . uientityElement $ before
        afterZ = uielementZIndex . uientityElement $ after
    case (uientityParent after, beforeZ /= afterZ) of
        (Just parent, True) -> void $ Component.modifyComponent store (`setUIEntityUpdated` True) parent
        _ -> return ()
    where
    store = uieventDispatcherElements dispatcher
modifyUIElement' False dispatcher f elemId = do
    r <- Component.modifyComponent store (modifyUIEntityElement f) elemId
    unless r . throwIO . userError $ "element not found: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher

readComponent :: Component.ComponentStore MBV.MVector UIElementId (UIEntity a) -> UIElementId -> IO (UIEntity a)
readComponent store eid = liftIO (maybe (throwIO . userError $ "element not found: " ++ show eid) return =<< Component.readComponent store eid)

setZIndex :: UIEventDispatcher a -> UIElementId -> Int -> IO ()
setZIndex dispatcher elemId z = modifyUIElement dispatcher f elemId
    where
    f a = a { uielementZIndex = z }

setCaptureHandler :: UIEventDispatcher a -> UIElementId -> CaptureHandler a -> IO ()
setCaptureHandler dispatcher elemId handler = modifyUIElement' False dispatcher f elemId
    where
    f a = a { uielementCaptureHandler = handler }

setBubbleHandler :: UIEventDispatcher a -> UIElementId -> BubbleHandler a -> IO ()
setBubbleHandler dispatcher elemId handler = modifyUIElement' False dispatcher f elemId
    where
    f a = a { uielementBubbleHandler = handler }

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
    root' <- liftIO $ readComponent store rootId
    go x y root'
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
    root' <- readComponent store rootId
    fromMaybe [] <$> go (V2 0 0) [] root'
    where
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherElements dispatcher

    go p0 xs entity = do
        r <- f p0 xs entity
        let p' = p0 ^+^ (locationPosition . uielementLocation . uientityElement $ entity)
        case r of
            Just (True, xs') -> do
                children <- getZSortedChildren dispatcher entity
                (`mplus` Just xs') . msum <$> BV.mapM (go p' xs') children
            Just (False, xs') -> return $ Just xs'
            Nothing -> return Nothing

    f p0 xs entity
        | uielementDisplay . uientityElement $ entity = do
            let handler = uielementCaptureHandler . uientityElement $ entity
            captureResult <- liftIO $ handler entity event p0
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
        compare (uielementZIndex . uientityElement $ e2) (uielementZIndex . uientityElement $ e1)
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
        Bubbled propagate (Just el) -> do
            _ <- Component.modifyComponent store (`updateUIEntityContent` el) (uientityId entity)
            if propagate
                then bubblePhase dispatcher es event target
                else return DispatchContinue
        BubbledExit ->
            return DispatchExit
    where
    handler = uielementBubbleHandler . uientityElement $ entity
    store = uieventDispatcherElements dispatcher

dispatchUIEvent :: UIEventDispatcher a -> UIEvent -> IO DispatchResult
dispatchUIEvent dispatcher event = do
    es <- capturePhase dispatcher event
    let target = if null es
                    then uieventDispatcherRoot dispatcher
                    else uientityId . head $ es
    bubblePhase dispatcher es event target

insideLocation :: V2 Int32 -> Location -> V2 Double -> Bool
insideLocation (V2 px py) (Location (V2 x y) (V2 width height)) (V2 x0 y0) =
    px' >= 0 && px' <= width && py' >= 0 && py' <= height
    where
    px' = fromIntegral px - (x0 + x)
    py' = fromIntegral py - (y0 + y)
