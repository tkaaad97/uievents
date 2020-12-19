{-# LANGUAGE FlexibleContexts #-}
module UIEvents
    ( module UIEvents.Types
    , newUIEventDispatcher
    , element
    , root
    , child
    , child_
    , addUIElement
    , removeUIElement
    , modifyUIElement
    , setUIElementZIndex
    , setUIElementCaptureHandler
    , setUIElementBubbleHandler
    , setFocus
    , releaseFocus
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
import Data.Maybe (fromMaybe, isJust, maybe)
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


updateUIEntityContent :: UIEntity a b -> UIElement a b -> UIEntity a b
updateUIEntityContent entity el = modifyUIEntityElement (const el) entity

setUIEntityUpdated :: UIEntity a b -> Bool -> UIEntity a b
setUIEntityUpdated a b = a { uientityUpdated = b }

modifyUIEntityElement :: (UIElement a b -> UIElement a b) -> UIEntity a b -> UIEntity a b
modifyUIEntityElement f entity = entity { uientityElement = f . uientityElement $ entity }

newUIEventDispatcher :: a -> b -> IO (UIEventDispatcher a b)
newUIEventDispatcher rootValue exitStatus = do
    let rootId = UIElementId 1
        rootElem = UIElement rootValue (Location (V2 0 0) (V2 0 0)) True 0 rootCapture rootBubble
        rootEntity = UIEntity rootId mempty Nothing rootElem False Nothing mempty False
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a b)))
    Component.addComponent store rootId rootEntity
    counter <- Counter.newCounter 1
    return (UIEventDispatcher counter rootId store)
    where
    rootCapture _ (UIEvent _ (WindowResizeEvent' _)) _ = return (Captured False)
    rootCapture _ (UIEvent _ WindowCloseEvent') _  = return (Captured False)
    rootCapture _ (UIEvent _ WindowEnterEvent') _      = return (Captured False)
    rootCapture _ (UIEvent _ WindowLeaveEvent') _      = return (Captured False)
    rootCapture entity (UIEvent _ (KeyboardEvent' _)) _
        | uientityFocused entity = return (Captured False)
        | isJust (uientityFocusedChild entity) = return (Captured True)
        | otherwise = return Uncaptured
    rootCapture _ _ _                                  = return (Captured True)
    rootBubble _ (UIEvent _ WindowCloseEvent') _ = return (BubbledExit exitStatus)
    rootBubble _ _ _ = return (Bubbled False Nothing)

addUIElement :: UIEventDispatcher a b -> UIElementId -> UIElement a b -> IO UIElementId
addUIElement dispatcher parent el = do
    elemId <- UIElementId <$> Counter.incrCounter 1 counter
    let entity = UIEntity elemId mempty (Just parent) el False Nothing mempty False
    Component.addComponent store elemId entity
    r <- Component.modifyComponent store (flip setUIEntityUpdated True . addChild elemId) parent
    unless r . throwIO . userError $ "element not found: " ++ show parent
    return elemId
    where
    counter = uieventDispatcherElementCounter dispatcher
    store = uieventDispatcherElements dispatcher
    addChild c e =
        e { uientityChildren = uientityChildren e `BV.snoc` c }

element :: a -> Location -> UIElement a b
element value location = e
    where
    e = UIElement value location True 0 ch bh
    ch _ (UIEvent _ (WindowResizeEvent' _)) _ = return Uncaptured
    ch _ (UIEvent _ WindowCloseEvent') _  = return Uncaptured
    ch _ (UIEvent _ WindowEnterEvent') _  = return Uncaptured
    ch _ (UIEvent _ WindowLeaveEvent') _  = return Uncaptured
    ch entity (UIEvent _ (MouseMotionEvent' ev)) p0
        | insideLocation (mouseMotionEventPosition ev) (uielementLocation . uientityElement $ entity) p0 = return (Captured True)
        | otherwise = return Uncaptured
    ch entity (UIEvent _ (MouseButtonEvent' ev)) p0
        | insideLocation (mouseButtonEventPosition ev) (uielementLocation . uientityElement $ entity) p0 = return (Captured True)
        | otherwise = return Uncaptured
    ch entity (UIEvent _ (KeyboardEvent' _)) _
        | uientityFocused entity = return (Captured False)
        | isJust (uientityFocusedChild entity) = return (Captured True)
        | otherwise = return Uncaptured
    bh _ _ _ = return (Bubbled True Nothing)

root :: ReaderT (UIEventDispatcher a b, UIElementId) IO c -> ReaderT (UIEventDispatcher a b) IO c
root block = do
    rid <- reader uieventDispatcherRoot
    withReaderT (flip (,) rid) block

child_ :: UIElement a b -> ReaderT (UIEventDispatcher a b, UIElementId) IO ()
child_ el = do
    (dispatcher, pid) <- ask
    _ <- liftIO $ addUIElement dispatcher pid el
    return ()

child :: UIElement a b -> ReaderT (UIEventDispatcher a b, UIElementId) IO c -> ReaderT (UIEventDispatcher a b, UIElementId) IO c
child el block = do
    (dispatcher, pid) <- ask
    eid <- liftIO $ addUIElement dispatcher pid el
    liftIO $ runReaderT block (dispatcher, eid)

setCaptureHandler :: CaptureHandler a b -> ReaderT (UIEventDispatcher a b, UIElementId) IO ()
setCaptureHandler handler = do
    (dispatcher, eid) <- ask
    _ <- liftIO $ setUIElementCaptureHandler dispatcher eid handler
    return ()

setBubbleHandler :: BubbleHandler a b -> ReaderT (UIEventDispatcher a b, UIElementId) IO ()
setBubbleHandler handler = do
    (dispatcher, eid) <- ask
    _ <- liftIO $ setUIElementBubbleHandler dispatcher eid handler
    return ()

modifyUIElement :: UIEventDispatcher a b -> (UIElement a b -> UIElement a b) -> UIElementId -> IO ()
modifyUIElement = modifyUIElement' True

modifyUIElement' :: Bool -> UIEventDispatcher a b -> (UIElement a b -> UIElement a b) -> UIElementId -> IO ()
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

readComponent :: Component.ComponentStore MBV.MVector UIElementId (UIEntity a b) -> UIElementId -> IO (UIEntity a b)
readComponent store eid = liftIO (maybe (throwIO . userError $ "element not found: " ++ show eid) return =<< Component.readComponent store eid)

setUIElementZIndex :: UIEventDispatcher a b -> UIElementId -> Int -> IO ()
setUIElementZIndex dispatcher elemId z = modifyUIElement dispatcher f elemId
    where
    f a = a { uielementZIndex = z }

setUIElementCaptureHandler :: UIEventDispatcher a b -> UIElementId -> CaptureHandler a b -> IO ()
setUIElementCaptureHandler dispatcher elemId handler = modifyUIElement' False dispatcher f elemId
    where
    f a = a { uielementCaptureHandler = handler }

setUIElementBubbleHandler :: UIEventDispatcher a b -> UIElementId -> BubbleHandler a b -> IO ()
setUIElementBubbleHandler dispatcher elemId handler = modifyUIElement' False dispatcher f elemId
    where
    f a = a { uielementBubbleHandler = handler }

setFocus :: UIEventDispatcher a b -> UIElementId -> IO ()
setFocus dispatcher elemId = do
    entity <- readComponent store elemId
    _ <- Component.modifyComponent store (\a -> a { uientityFocused = True }) elemId
    updateParent elemId (uientityParent entity)
    where
    store = uieventDispatcherElements dispatcher
    updateParent cid (Just pid) = do
        parent <- readComponent store pid
        _ <- Component.modifyComponent store (\a -> a { uientityFocusedChild = Just cid }) pid
        updateParent pid (uientityParent parent)
    updateParent _ Nothing = return ()

releaseFocus :: UIEventDispatcher a b -> IO ()
releaseFocus dispatcher = do
    entity <- readComponent store rootId
    go rootId (uientityFocused entity) (uientityFocusedChild entity)
    where
    store = uieventDispatcherElements dispatcher
    rootId = uieventDispatcherRoot dispatcher
    go eid _ (Just cid) = do
        _ <- Component.modifyComponent store (\a -> a { uientityFocused = False, uientityFocusedChild = Nothing }) eid
        c <- readComponent store cid
        go cid (uientityFocused c) (uientityFocusedChild c)
    go eid True Nothing = do
        _ <- Component.modifyComponent store (\a -> a { uientityFocused = False }) eid
        return ()
    go _ False Nothing = return ()

removeUIElement :: UIEventDispatcher a b -> UIElementId -> IO ()
removeUIElement dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    BV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
    parentModified <- maybe (return False) (Component.modifyComponent store removeChild) $ uientityParent e
    unless parentModified . throwIO . userError $ "parent element not found"
    deleted <- Component.removeComponent store elemId
    unless deleted . throwIO . userError $ "delete failed: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher
    removeChild e = e
        { uientityChildren = BV.filter (/= elemId) $ uientityChildren e
        , uientityUpdated = True
        }

removeUIElement_ :: UIEventDispatcher a b -> UIElementId -> IO ()
removeUIElement_ dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    BV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
    deleted <- Component.removeComponent store elemId
    unless deleted . throwIO . userError $ "delete failed: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher

sliceUIEntities :: UIEventDispatcher a b -> IO (BV.Vector (UIEntity a b))
sliceUIEntities dispatcher = do
    (v, n) <- Component.unsafeGetComponentVector store
    return $ BV.take n v
    where
    store = uieventDispatcherElements dispatcher

foldUIEntities :: MonadIO m => UIEventDispatcher a b -> (UIEntity a b -> x -> y -> m (x, y)) -> x -> y -> m y
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

capturePhase :: UIEventDispatcher a b -> UIEvent -> IO [UIEntity a b]
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

getZSortedChildren :: UIEventDispatcher a b -> UIEntity a b -> IO (BV.Vector (UIEntity a b))
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

bubblePhase :: UIEventDispatcher a b -> [UIEntity a b] -> UIEvent -> UIElementId -> IO (DispatchResult b)
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
        BubbledExit b ->
            return (DispatchExit b)
    where
    handler = uielementBubbleHandler . uientityElement $ entity
    store = uieventDispatcherElements dispatcher

dispatchUIEvent :: UIEventDispatcher a b -> UIEvent -> IO (DispatchResult b)
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
