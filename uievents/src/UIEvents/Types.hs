{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UIEvents.Types
    ( UIElementId(..)
    , UIEvent(..)
    , UIEventPayload(..)
    , Location(..)
    , CaptureResult(..)
    , BubbleResult(..)
    , DispatchResult(..)
    , CaptureHandler
    , BubbleHandler
    , Timestamp(..)
    , UIEntity(..)
    , UIElement(..)
    , UIEventDispatcher(..)
    , WindowResizeEvent(..)
    , MouseMotionEvent(..)
    , MouseButtonEvent(..)
    , MouseButtonEventType(..)
    , MouseButton(..)
    , KeyboardEvent(..)
    , KeyboardEventType(..)
    , Key(..)
    ) where

import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import qualified Data.Vector as BV (Vector)
import qualified Data.Vector.Mutable as MBV (IOVector)
import Foreign (Storable)
import Linear (V2(..))

newtype UIElementId = UIElementId
    { unUIElementId :: Int
    } deriving (Show, Eq, Ord, Enum, Bounded, Hashable, Num, Storable)

newtype Timestamp = Timestamp
    { unTimestamp :: Int64
    } deriving (Show, Eq)

data UIEvent = UIEvent
    { uieventTimestamp :: !Timestamp
    , uieventPayload   :: !UIEventPayload
    } deriving (Show, Eq)

data UIEventPayload =
    WindowResizeEvent' WindowResizeEvent |
    WindowCloseEvent' |
    WindowEnterEvent' |
    WindowLeaveEvent' |
    MouseMotionEvent' MouseMotionEvent |
    MouseButtonEvent' MouseButtonEvent |
    KeyboardEvent' KeyboardEvent
    deriving (Show, Eq)

newtype WindowResizeEvent = WindowResizeEvent
    { windowResizeEventSize :: V2 Int32
    } deriving (Show, Eq)

data MouseMotionEvent = MouseMotionEvent
    { mouseMotionEventPosition :: !(V2 Int32)
    , mouseMotionEventMovement :: !(V2 Int32)
    , mouseMotionEventButtons  :: ![MouseButton]
    } deriving (Show, Eq)

data MouseButtonEvent = MouseButtonEvent
    { mouseButtonEventType     :: !MouseButtonEventType
    , mouseButtonEventButton   :: !MouseButton
    , mouseButtonEventPosition :: !(V2 Int32)
    } deriving (Show, Eq)

data MouseButton =
    MouseButtonLeft |
    MouseButtonMiddle |
    MouseButtonRight |
    MouseButtonExtra !Int32
    deriving (Show, Eq)

data MouseButtonEventType =
    MouseButtonPressed |
    MouseButtonReleased
    deriving (Show, Eq, Enum)

data KeyboardEvent = KeyboardEvent
    { keyboardEventType :: !KeyboardEventType
    , keyboardEventKey  :: !Key
    } deriving (Show, Eq)

newtype Key = Key
    { unKey :: Int32
    } deriving (Show, Eq)

data KeyboardEventType =
    KeyPressed |
    KeyRepeated |
    KeyReleased
    deriving (Show, Eq)

data Location = Location
    { locationPosition :: !(V2 Double)
    , locationSize     :: !(V2 Double)
    } deriving (Show, Eq)

data CaptureResult =
    Captured !Bool |
    Uncaptured
    deriving (Show, Eq)

data BubbleResult a b =
    Bubbled !Bool !(Maybe (UIElement a b)) |
    BubbledExit b
    deriving (Show)

data DispatchResult b =
    DispatchContinue |
    DispatchExit !b
    deriving (Show, Eq)

type CaptureHandler a b = UIEntity a b -> UIEvent -> V2 Double -> IO CaptureResult
type BubbleHandler a b = UIEntity a b -> UIEvent -> UIElementId -> IO (BubbleResult a b)

data UIEntity a b = UIEntity
    { uientityId              :: !UIElementId
    , uientityChildren        :: !(BV.Vector UIElementId)
    , uientityParent          :: !(Maybe UIElementId)
    , uientityElement         :: !(UIElement a b)
    , uientityFocused         :: !Bool
    , uientityFocusedChild    :: !(Maybe UIElementId)
    , uientityZSortedChildren :: !(BV.Vector UIElementId)
    , uientityUpdated         :: !Bool
    } deriving (Show)

data UIElement a b = UIElement
    { uielementValue          :: !a
    , uielementLocation       :: !Location
    , uielementDisplay        :: !Bool
    , uielementZIndex         :: !Int
    , uielementCaptureHandler :: !(CaptureHandler a b)
    , uielementBubbleHandler  :: !(BubbleHandler a b)
    }

instance Show (UIElement a b) where
    show _ = "UIElement {}"

data UIEventDispatcher a b = UIEventDispatcher
    { uieventDispatcherEntities :: !(MBV.IOVector (UIEntity a b))
    , uieventDispatcherRoot     :: !UIElementId
    }
