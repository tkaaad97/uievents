{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UIEvents.Types
    ( UIElementId(..)
    , UIEvent(..)
    , Location(..)
    , CaptureHandler
    , TargetHandler
    , BubbleHandler
    , UIEntity(..)
    , UIElement(..)
    , UIElementHandlers(..)
    , UIEventDispatcher(..)
    ) where

import Data.Atomics.Counter (AtomicCounter)
import Data.Hashable (Hashable)
import qualified Data.Vector.Mutable as MBV (MVector)
import qualified Data.Vector.Storable as SV (Vector)
import Foreign (Storable)
import Linear (V2(..))
import qualified UIEvents.Internal.Component as Component (ComponentStore)

newtype UIElementId = UIElementId Int
    deriving (Show, Eq, Ord, Enum, Bounded, Hashable, Num, Storable)

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
    { uieventDispatcherElementCounter :: !AtomicCounter
    , uieventDispatcherRoot     :: !UIElementId
    , uieventDispatcherElements :: !(Component.ComponentStore MBV.MVector UIElementId (UIEntity a b))
    }

instance Show (UIElementHandlers a b) where
    show _ = "UIElementHandlers {}"
