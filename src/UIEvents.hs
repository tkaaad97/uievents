module UIEvents
    ( newUIEventDispatcher
    , addUIElement
    , removeUIElement
    , sliceUIEntities
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import qualified Data.Atomics.Counter as Counter (incrCounter, newCounter)
import qualified UIEvents.Internal.Component as Component (addComponent,
                                                           modifyComponent,
                                                           newComponentStore,
                                                           readComponent,
                                                           removeComponent,
                                                           unsafeGetComponentVector)

import Data.Proxy (Proxy(..))
import qualified Data.Vector as BV (Vector, take)
import qualified Data.Vector.Mutable as MBV (IOVector)
import qualified Data.Vector.Storable as SV (filter, mapM_, snoc)
import UIEvents.Types

newUIEventDispatcher :: Proxy a -> UIElement b -> IO (UIEventDispatcher a b)
newUIEventDispatcher _ rootElem = do
    let rootId = UIElementId 1
        rootEntity = UIEntity rootId mempty Nothing rootElem rootHandlers
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a b)))
    Component.addComponent store rootId rootEntity
    counter <- Counter.newCounter 1
    return (UIEventDispatcher counter rootId store)
    where
    rootHandlers = UIElementHandlers rootCapture rootTarget rootBubble
    rootCapture _ _ = return True
    rootTarget _ _ = return Nothing
    rootBubble _ _ = return False

addUIElement :: UIEventDispatcher a b -> UIElementId -> UIElement b -> UIElementHandlers a b -> IO UIElementId
addUIElement dispatcher parent element handlers = do
    elemId <- UIElementId <$> Counter.incrCounter 1 counter
    let entity = UIEntity elemId mempty (Just parent) element handlers
    Component.addComponent store elemId entity
    r <- Component.modifyComponent store (addChild elemId) parent
    when r . throwIO . userError $ "element not found: " ++ show parent
    return elemId
    where
    counter = uieventDispatcherElementCounter dispatcher
    store = uieventDispatcherElements dispatcher
    addChild c e =
        e { uientityChildren = uientityChildren e `SV.snoc` c }

removeUIElement :: UIEventDispatcher a b -> UIElementId -> IO ()
removeUIElement dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    SV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
    parentModified <- maybe (return False) (Component.modifyComponent store removeChild) $ uientityParent e
    unless parentModified . throwIO . userError $ "parent element not found"
    deleted <- Component.removeComponent store elemId
    unless deleted . throwIO . userError $ "delete failed: " ++ show elemId
    where
    store = uieventDispatcherElements dispatcher
    removeChild e =
        e { uientityChildren = SV.filter (/= elemId) $ uientityChildren e }

removeUIElement_ :: UIEventDispatcher a b -> UIElementId -> IO ()
removeUIElement_ dispatcher elemId = do
    e <- maybe (throwIO . userError $ "element not found: " ++ show elemId) return =<< Component.readComponent store elemId
    SV.mapM_ (removeUIElement_ dispatcher) . uientityChildren $ e
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

--capturePhase :: UIEventDispatcher -> UIEvent -> IO (Maybe (UIElement a))
--targetPhase
--bubblePhase
--modifyUIElement
--unsafeSliceUIElement
--clearUIElements
--foldUIElements :: UIEventDispather -> (UIEntity -> a -> b -> MaybeT IO (a, b)) -> a -> b -> MaybeT IO b
