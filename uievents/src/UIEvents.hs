{-# LANGUAGE FlexibleContexts #-}
module UIEvents
    ( module UIEvents.Builder
    , BubbleHandler
    , BubbleResult(..)
    , CaptureHandler
    , CaptureResult(..)
    , DispatchResult(..)
    , Key(..)
    , KeyboardEvent(..)
    , KeyboardEventType(..)
    , Location(..)
    , MouseButton(..)
    , MouseButtonEvent(..)
    , MouseButtonEventType(..)
    , MouseMotionEvent(..)
    , Timestamp(..)
    , UIElement(..)
    , UIElementId
    , UIEntity(..)
    , UIEvent(..)
    , UIEventDispatcher
    , UIEventPayload(..)
    , WindowResizeEvent(..)
    , modifyUIElement
    , setUIElementZIndex
    , setUIElementCaptureHandler
    , setUIElementBubbleHandler
    , setFocus
    , releaseFocus
    , sliceUIEntities
    , foldUIEntities
    , dispatchUIEvent
    ) where

import Control.Monad (mplus, msum)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as BV (Vector, foldM', freeze, imapM_, length, map,
                                    mapM, unsafeFreeze)
import qualified Data.Vector.Algorithms.Intro as VA (sortBy)
import qualified Data.Vector.Mutable as MBV (modify, new, read, write)
import Linear (V2(..), (^+^))
import UIEvents.Builder
import UIEvents.Types

updateUIEntityContent :: UIEntity a b -> UIElement a b -> UIEntity a b
updateUIEntityContent entity el = modifyUIEntityElement (const el) entity

setUIEntityUpdated :: UIEntity a b -> Bool -> UIEntity a b
setUIEntityUpdated a b = a { uientityUpdated = b }

modifyUIElement :: UIEventDispatcher a b -> (UIElement a b -> UIElement a b) -> UIElementId -> IO ()
modifyUIElement = modifyUIElement' True

modifyUIElement' :: Bool -> UIEventDispatcher a b -> (UIElement a b -> UIElement a b) -> UIElementId -> IO ()
modifyUIElement' True dispatcher f (UIElementId elemId) = do
    before <- MBV.read store elemId
    let after = modifyUIEntityElement f before
    MBV.write store elemId after
    let beforeZ = uielementZIndex . uientityElement $ before
        afterZ = uielementZIndex . uientityElement $ after
    case (uientityParent after, beforeZ /= afterZ) of
        (Just (UIElementId parent), True) -> MBV.modify store (`setUIEntityUpdated` True) parent
        _ -> return ()
    where
    store = uieventDispatcherEntities dispatcher

modifyUIElement' False dispatcher f (UIElementId elemId) =
    MBV.modify store (modifyUIEntityElement f) elemId
    where
    store = uieventDispatcherEntities dispatcher

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
    entity <- MBV.read store (unUIElementId elemId)
    MBV.modify store (\a -> a { uientityFocused = True }) (unUIElementId elemId)
    updateParent elemId (uientityParent entity)
    where
    store = uieventDispatcherEntities dispatcher
    updateParent :: UIElementId -> Maybe UIElementId -> IO ()
    updateParent cid (Just pid) = do
        parent <- MBV.read store (unUIElementId pid)
        MBV.modify store (\a -> a { uientityFocusedChild = Just cid }) (unUIElementId pid)
        updateParent pid (uientityParent parent)
    updateParent _ Nothing = return ()

releaseFocus :: UIEventDispatcher a b -> IO ()
releaseFocus dispatcher = do
    entity <- MBV.read store (unUIElementId rootId)
    go rootId (uientityFocused entity) (uientityFocusedChild entity)
    where
    store = uieventDispatcherEntities dispatcher
    rootId = uieventDispatcherRoot dispatcher
    go :: UIElementId -> Bool -> Maybe UIElementId -> IO ()
    go eid _ (Just cid) = do
        MBV.modify store (\a -> a { uientityFocused = False, uientityFocusedChild = Nothing }) (unUIElementId eid)
        c <- MBV.read store (unUIElementId cid)
        go cid (uientityFocused c) (uientityFocusedChild c)
    go eid True Nothing = do
        MBV.modify store (\a -> a { uientityFocused = False }) (unUIElementId eid)
        return ()
    go _ False Nothing = return ()

sliceUIEntities :: UIEventDispatcher a b -> IO (BV.Vector (UIEntity a b))
sliceUIEntities dispatcher = BV.freeze store
    where
    store = uieventDispatcherEntities dispatcher

foldUIEntities :: MonadIO m => UIEventDispatcher a b -> (UIEntity a b -> x -> y -> m (x, y)) -> x -> y -> m y
foldUIEntities dispatcher f x y = do
    root' <- liftIO $ MBV.read store (unUIElementId rootId)
    go x y root'
    where
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherEntities dispatcher
    go x0 y0 entity = do
        (x1, y1) <- f entity x0 y0
        let elemIds = uientityChildren entity
        entities <- liftIO $ BV.mapM (MBV.read store . unUIElementId) elemIds
        BV.foldM' (go x1) y1 entities

capturePhase :: UIEventDispatcher a b -> UIEvent -> IO [UIEntity a b]
capturePhase dispatcher event = do
    root' <- MBV.read store (unUIElementId rootId)
    fromMaybe [] <$> go (V2 0 0) [] root'
    where
    rootId = uieventDispatcherRoot dispatcher
    store = uieventDispatcherEntities dispatcher

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
        BV.imapM_ (\i e -> MBV.read store (unUIElementId e) >>= MBV.write entities i) children
        VA.sortBy compareZIndexDesc entities
        freezed <- BV.unsafeFreeze entities
        let zsortedChildren = BV.map uientityId freezed
        MBV.modify store (`updateZSortedChildren` zsortedChildren) (unUIElementId . uientityId $ entity)
        return freezed
    | otherwise =
        BV.mapM (MBV.read store . unUIElementId) (uientityZSortedChildren entity)
    where
    store = uieventDispatcherEntities dispatcher
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
            MBV.modify store (`updateUIEntityContent` el) (unUIElementId . uientityId $ entity)
            if propagate
                then bubblePhase dispatcher es event target
                else return DispatchContinue
        BubbledExit b ->
            return (DispatchExit b)
    where
    handler = uielementBubbleHandler . uientityElement $ entity
    store = uieventDispatcherEntities dispatcher

dispatchUIEvent :: UIEventDispatcher a b -> UIEvent -> IO (DispatchResult b)
dispatchUIEvent dispatcher event = do
    es <- capturePhase dispatcher event
    let target = if null es
                    then uieventDispatcherRoot dispatcher
                    else uientityId . head $ es
    bubblePhase dispatcher es event target
