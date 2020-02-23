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
import Data.Int (Int32, Int64)
import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Mutable as MBV (MVector)
import Foreign (Storable)
import Linear (V2(..))
import qualified UIEvents.Internal.Component as Component (ComponentStore)

newtype UIElementId = UIElementId Int
    deriving (Show, Eq, Ord, Enum, Bounded, Hashable, Num, Storable)

data UIEvent = UIEvent
    { uieventTimestamp :: !Int64
    , uieventPayload   :: !UIEventPayload
    } deriving (Show, Eq)

data UIEventPayload =
    WindowResizeEvent' WindowResizeEvent |
    WindowCloseEvent' WindowCloseEvent |
    MouseMotionEvent' MouseMotionEvent |
    MouseButtonEvent' MouseButtonEvent |
    KeyboardEvent' KeyboardEvent
    deriving (Show, Eq)

newtype WindowResizeEvent = WindowResizeEvent
    { windowResizeEventSize :: V2 Int32
    } deriving (Show, Eq)

data WindowCloseEvent = WindowCloseEvent
    deriving (Show, Eq)

data MouseMotionEvent = MouseMotionEvent
    { mouseMotionEventPosition :: !(V2 Int32)
    , mouseMotionEventMovement :: !(V2 Int32)
    , mouseMotionEventButtons  :: ![MouseButton]
    } deriving (Show, Eq)

data MouseButtonEvent = MouseButtonEvent
    { mouseButtonEventType     :: !MouseButtonEventType
    , mouseButtonEventButton   :: !MouseButton
    , mouseButtonEevntPosition :: !(V2 Int32)
    } deriving (Show, Eq)

data MouseButton =
    MouseButtonLeft |
    MouseButtonMiddle |
    MouseButtonRight |
    MouseButtonOther !Int32
    deriving (Show, Eq)

data MouseButtonEventType =
    MouseButtonPressed |
    MouseButtonReleased
    deriving (Show, Eq, Enum)

data KeyboardEvent = KeyboardEvent
    { keyboardEventType :: !KeyboardEventType
    , keyboardEventKey  :: !Keycode
    } deriving (Show, Eq)

newtype Keycode = Keycode
    { unKeycode :: Int32
    } deriving (Show, Eq)

data KeyboardEventType =
    KeyPressed |
    KeyRepeated |
    KeyReleased
    deriving (Show, Eq)

data Location = Location
    { locationPosition :: !(V2 Double)
    , locationSize     :: !(V2 Double)
    , locationRotation :: !Double
    } deriving (Show, Eq)

type CaptureHandler a = UIEntity a -> UIEvent -> IO Bool
type TargetHandler a = UIEntity a -> UIEvent -> IO (Maybe (UIElement a))
type BubbleHandler a = UIEntity a -> UIEvent -> IO Bool

data UIEntity a = UIEntity
    { uientityId       :: !UIElementId
    , uientityChildren :: !(BV.Vector UIElementId)
    , uientityParent   :: !(Maybe UIElementId)
    , uientityContent  :: !(UIElement a)
    , uientityHandlers :: !(UIElementHandlers a)
    } deriving (Show)

data UIElement b = UIElement
    { uielementLocation :: !Location
    , uielementValue    :: !b
    } deriving (Show, Eq)

data UIElementHandlers a = UIElementHandlers
    { captureHandler :: !(CaptureHandler a)
    , targetHandler  :: !(TargetHandler a)
    , bubbleHandler  :: !(BubbleHandler a)
    }

data UIEventDispatcher a = UIEventDispatcher
    { uieventDispatcherElementCounter :: !AtomicCounter
    , uieventDispatcherRoot     :: !UIElementId
    , uieventDispatcherElements :: !(Component.ComponentStore MBV.MVector UIElementId (UIEntity a))
    }

instance Show (UIElementHandlers a) where
    show _ = "UIElementHandlers {}"
