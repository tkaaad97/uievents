module UIEvents.GLFW
    (
    ) where

import qualified Chronos as Time (Time(..), now)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Graphics.UI.GLFW as GLFW (CursorState, Error, Joystick,
                                           JoystickState, Key, KeyState,
                                           ModifierKeys, Monitor,
                                           MonitorCallback, MonitorState,
                                           MouseButton, MouseButtonState,
                                           Window, WindowPosCallback)
import qualified UIEvents

data GLFWEventPayload =
    GLFWErrorEvent !GLFW.Error !String |
    GLFWMonitorEvent !GLFW.Monitor !GLFW.MonitorState |
    GLFWWindowPosEvent !GLFW.Window !Int !Int |
    GLFWWindowSizeEvent !GLFW.Window !Int !Int |
    GLFWWindowCloseEvent !GLFW.Window |
    GLFWWindowRefreshEvent !GLFW.Window |
    GLFWWindowFocusEvent !GLFW.Window !Bool |
    GLFWWindowIconifyEvent !GLFW.Window !Bool |
    GLFWFramebufferSizeEvent !GLFW.Window !Int !Int |
    GLFWWindowContentScaleEvent !GLFW.Window !Float !Float |
    GLFWWindowMaximizeEvent !GLFW.Window !Bool |
    GLFWKeyEvent !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys |
    GLFWCharEvent !GLFW.Window !Char |
    GLFWCharModsEvent !GLFW.Window !Char !GLFW.ModifierKeys |
    GLFWMouseButtonEvent !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys |
    GLFWCursorPosEvent !GLFW.Window !Double !Double |
    GLFWCursorEnterEvent !GLFW.Window !GLFW.CursorState |
    GLFWScrollEvent !GLFW.Window !Double !Double |
    GLFWDropEvent !GLFW.Window ![String] |
    GLFWJoystickEvent !GLFW.Joystick !GLFW.JoystickState
    deriving (Show, Eq)

data GLFWEvent = GLFWEvent
    { glfwEventTimestamp :: !UIEvents.Timestamp
    , glfwEventPayload   :: !GLFWEventPayload
    } deriving (Show, Eq)

enqueueEvent :: GLFWEvent -> [GLFWEvent] -> ([GLFWEvent], ())
enqueueEvent event queue = (event : queue, ())

enqueuePayloadNow :: IORef [GLFWEvent] -> GLFWEventPayload -> IO ()
enqueuePayloadNow queue payload = do
    t <- UIEvents.Timestamp . Time.getTime <$> Time.now
    let event = GLFWEvent t payload
    atomicModifyIORef' queue (enqueueEvent event)

enqueueMonitorEvent :: IORef [GLFWEvent] -> GLFW.MonitorCallback
enqueueMonitorEvent queue monitor monitorState =
    enqueuePayloadNow queue $ GLFWMonitorEvent monitor monitorState

enqueueWindowPosEvent :: IORef [GLFWEvent] -> GLFW.WindowPosCallback
enqueueWindowPosEvent queue window x y =
    enqueuePayloadNow queue $ GLFWWindowPosEvent window x y

{-
enqueueWindowPosEvent
enqueueWindowSizeEvent
enqueueWindowCloseEvent
enqueueWindowRefreshEvent
enqueueWindowFocusEvent
enqueueWindowIconifyEvent
enqueueFramebufferSizeEvent
enqueueWindowContentScaleEvent
enqueueWindowMaximizeEvent
enqueueKeyEvent
enqueueCharEvent
enqueueCharModsEvent
enqueueMouseButtonEvent
enqueueCursorPosEvent
enqueueCursorEnterEvent
enqueueScrollEvent
enqueueDropEvent
enqueueJoystickEvent

setCallbacks :: IORef [UIEvent] -> IO ()
setCallbacks eventQueue = do
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
    GLFW.setMonitorCallback (Just (monitorCallback eventQueue))
-}
