{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UIEvents.Builder
    ( Builder
    , newUIEventDispatcher
    , getCurrent
    , element
    , child
    , child_
    , modify
    , setCaptureHandler
    , setBubbleHandler
    , addPostBuildHook
    , modifyUIEntityElement
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State (execStateT, get,
                                                            modify, put)
import Data.Int (Int32)
import Data.Maybe (isJust)
import qualified Data.Vector as BV (snoc)
import qualified Data.Vector.Mutable as MBV (IOVector, grow, length, modify,
                                             new, take, write)
import Linear (V2(..))
import UIEvents.Types

type BuildHook a b = UIEventDispatcher a b -> IO ()

data BuilderS a b = BuilderS !(MBV.IOVector (UIEntity a b), Int) !UIElementId ![BuildHook a b]

newtype Builder a b c = Builder
    { runBuilder :: StateT (BuilderS a b) IO c
    } deriving (Functor, Applicative, Monad, MonadIO)

newUIEventDispatcher :: a -> b -> Builder a b () -> IO (UIEventDispatcher a b)
newUIEventDispatcher rootValue exitStatus builder = do
    let rootId = UIElementId 0
        rootElem = UIElement rootValue (Location (V2 0 0) (V2 0 0)) True 0 rootCapture rootBubble
        rootEntity = UIEntity rootId mempty Nothing rootElem False Nothing mempty False
    store <- MBV.new 10
    MBV.write store (unUIElementId rootId) rootEntity
    BuilderS (store', len) _ hooks <- State.execStateT (runBuilder builder) (BuilderS (store, 1) rootId mempty)
    let entities = MBV.take len store'
        dispatcher = UIEventDispatcher entities rootId
    mapM_ ($ dispatcher) . reverse $ hooks
    return dispatcher
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

addUIElement :: (MBV.IOVector (UIEntity a b), Int) -> UIElementId -> UIElement a b -> IO ((MBV.IOVector (UIEntity a b), Int), UIElementId)
addUIElement (entities, len) parent el = do
    let elemId = UIElementId len
        entity = UIEntity elemId mempty (Just parent) el False Nothing mempty False
    (entities', len') <- pushBack (entities, len) entity
    MBV.modify entities' (flip setUIEntityUpdated True . addChild elemId) (unUIElementId parent)
    return ((entities', len'), elemId)
    where
    addChild c e =
        e { uientityChildren = uientityChildren e `BV.snoc` c }

    pushBack :: (MBV.IOVector (UIEntity a b), Int) -> UIEntity a b -> IO (MBV.IOVector (UIEntity a b), Int)
    pushBack (v, l) a
        | l < MBV.length v = do
            MBV.write v l a
            return (v, l + 1)
        | otherwise = do
            v' <- MBV.grow v (MBV.length v * 2)
            pushBack (v', l) a

    setUIEntityUpdated a b = a { uientityUpdated = b }

getCurrent :: Builder a b UIElementId
getCurrent = do
    BuilderS _ current _ <- Builder State.get
    return current

child_ :: UIElement a b -> Builder a b UIElementId
child_ el = do
    BuilderS v parent hooks <- Builder State.get
    (v', eid) <- liftIO $ addUIElement v parent el
    Builder $ State.put (BuilderS v' parent hooks)
    return eid

child :: UIElement a b -> (UIElementId -> Builder a b c) -> Builder a b c
child el block = do
    BuilderS v parent hooks <- Builder State.get
    (v', cid) <- liftIO $ addUIElement v parent el
    Builder $ State.put (BuilderS v' cid hooks)
    c <- block cid
    Builder $ State.modify $ \(BuilderS a _ hooks') -> BuilderS a parent hooks'
    return c

modify :: (UIElement a b -> UIElement a b) -> UIElementId -> Builder a b ()
modify f eid = do
    BuilderS (v, _) _ _ <- Builder State.get
    liftIO $ MBV.modify v (modifyUIEntityElement f) (unUIElementId eid)

setCaptureHandler :: CaptureHandler a b -> Builder a b ()
setCaptureHandler handler = do
    BuilderS _ current _ <- Builder State.get
    modify f current
    where
    f a = a { uielementCaptureHandler = handler }

setBubbleHandler :: BubbleHandler a b -> Builder a b ()
setBubbleHandler handler = do
    BuilderS _ current _ <- Builder State.get
    modify f current
    where
    f a = a { uielementBubbleHandler = handler }

addPostBuildHook :: (UIEventDispatcher a b -> IO ()) -> Builder a b ()
addPostBuildHook hook =
    Builder . State.modify $ \(BuilderS v current hooks) -> (BuilderS v current (hook : hooks))

modifyUIEntityElement :: (UIElement a b -> UIElement a b) -> UIEntity a b -> UIEntity a b
modifyUIEntityElement f entity = entity { uientityElement = f . uientityElement $ entity }

insideLocation :: V2 Int32 -> Location -> V2 Double -> Bool
insideLocation (V2 px py) (Location (V2 x y) (V2 width height)) (V2 x0 y0) =
    px' >= 0 && px' <= width && py' >= 0 && py' <= height
    where
    px' = fromIntegral px - (x0 + x)
    py' = fromIntegral py - (y0 + y)
