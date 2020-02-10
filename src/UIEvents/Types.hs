{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UIEvents.Types
    (
    ) where

import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Mutable as MBV (IOVector, MVector)
import qualified Data.Vector.Storable as SV (Vector)
import Foreign (Storable)
import Linear (V2(..))
import qualified UIEvents.Internal.Component as Component (ComponentStore,
                                                           addComponent,
                                                           newComponentStore)

newtype UIElementId = UIElementId Int
    deriving (Show, Eq, Ord, Enum, Hashable, Num, Storable)

data UIEvent a = UIEvent a
    deriving (Show, Eq)

data Location = Location
    { locationPosition :: !(V2 Double)
    , locationSize     :: !(V2 Double)
    } deriving (Show, Eq)

type CaptureHandler a b = UIEntity a b -> UIEvent a -> IO Bool
type TargetHandler a b = UIEntity a b -> UIEvent a -> IO (Maybe (UIElement b))
type BubbleHandler a b = UIEntity a b -> UIEvent a -> IO Bool

data UIEntity a b = UIEntity
    { uientityId       :: !UIElementId
    , uientityChildren :: !(SV.Vector UIElementId)
    , uientityParent   :: !(Maybe UIElementId)
    , uientityContent  :: !(UIElement b)
    , uientityHandlers :: !(UIElementHandlers a b)
    } deriving (Show)

data UIElement b = UIElement
    { uielementLocation :: !Location
    , uielementValue    :: !b
    } deriving (Show, Eq)

data UIElementHandlers a b = UIElementHandlers
    { captureHandler :: !(CaptureHandler a b)
    , targetHandler  :: !(TargetHandler a b)
    , bubbleHandler  :: !(BubbleHandler a b)
    }

data UIEventDispatcher a b = UIEventDispatcher
    { uieventDispatcherRoot     :: !UIElementId
    , uieventDispatcherElements :: !(Component.ComponentStore MBV.MVector UIElementId (UIEntity a b))
    , uieventDispatcherElementCounter :: !(IORef UIElementId)
    }

instance Show (UIElementHandlers a b) where
    show _ = "UIElementHandlers {}"

newUIEventDispatcher :: Proxy a -> UIElement b -> IO (UIEventDispatcher a b)
newUIEventDispatcher _ rootElem = do
    let rootId = UIElementId 1
        rootEntity = UIEntity rootId mempty Nothing rootElem rootHandlers
        counter = UIElementId 2
    store <- Component.newComponentStore 10 (Proxy :: Proxy (MBV.IOVector (UIEntity a b)))
    counterRef <- newIORef counter
    Component.addComponent store rootId rootEntity
    return (UIEventDispatcher rootId store counterRef)
    where
    rootHandlers = UIElementHandlers rootCapture rootTarget rootBubble
    rootCapture _ _ = return True
    rootTarget _ _ = return Nothing
    rootBubble _ _ = return False

--capturePhase :: UIEventDispatcher -> UIEvent -> IO (Maybe (UIElement a))
--targetPhase
--bubblePhase
--addUIElement
--removeUIElement
--sliceUIElement
--modifyUIElement
--unsafeSliceUIElement
--clearUIElements
--foldUIElements :: UIEventDispather -> (UIEntity -> a -> b -> MaybeT IO (a, b)) -> a -> b -> MaybeT IO b
