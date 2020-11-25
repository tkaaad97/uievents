module UIEvents.GLFW
    (
    ) where

import qualified Chronos as Time (Time(..), now)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Graphics.UI.GLFW as GLFW
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

enqueueWindowSizeEvent :: IORef [GLFWEvent] -> GLFW.WindowSizeCallback
enqueueWindowSizeEvent queue window w h =
    enqueuePayloadNow queue $ GLFWWindowSizeEvent window w h

enqueueWindowCloseEvent :: IORef [GLFWEvent] -> GLFW.WindowCloseCallback
enqueueWindowCloseEvent queue window =
    enqueuePayloadNow queue $ GLFWWindowCloseEvent window

enqueueWindowRefreshEvent :: IORef [GLFWEvent] -> GLFW.WindowCloseCallback
enqueueWindowRefreshEvent queue window =
    enqueuePayloadNow queue $ GLFWWindowRefreshEvent window

enqueueWindowFocusEvent :: IORef [GLFWEvent] -> GLFW.WindowFocusCallback
enqueueWindowFocusEvent queue window focus =
    enqueuePayloadNow queue $ GLFWWindowFocusEvent window focus

enqueueWindowIconifyEvent :: IORef [GLFWEvent] -> GLFW.WindowIconifyCallback
enqueueWindowIconifyEvent queue window iconify =
    enqueuePayloadNow queue $ GLFWWindowFocusEvent window iconify

enqueueFramebufferSizeEvent :: IORef [GLFWEvent] -> GLFW.FramebufferSizeCallback
enqueueFramebufferSizeEvent queue window w h =
    enqueuePayloadNow queue $ GLFWFramebufferSizeEvent window w h

enqueueWindowContentScaleEvent :: IORef [GLFWEvent] -> GLFW.WindowContentScaleCallback
enqueueWindowContentScaleEvent queue window sx sy =
    enqueuePayloadNow queue $ GLFWWindowContentScaleEvent window sx sy

enqueueWindowMaximizeEvent :: IORef [GLFWEvent] -> GLFW.WindowMaximizeCallback
enqueueWindowMaximizeEvent queue window maximize =
    enqueuePayloadNow queue $ GLFWWindowMaximizeEvent window maximize

enqueueKeyEvent :: IORef [GLFWEvent] -> GLFW.KeyCallback
enqueueKeyEvent queue window key scan keyState modifierKeys =
    enqueuePayloadNow queue $ GLFWKeyEvent window key scan keyState modifierKeys

enqueueCharEvent :: IORef [GLFWEvent] -> GLFW.CharCallback
enqueueCharEvent queue window char =
    enqueuePayloadNow queue $ GLFWCharEvent window char

enqueueCharModsEvent :: IORef [GLFWEvent] -> GLFW.CharModsCallback
enqueueCharModsEvent queue window char modifierKeys =
    enqueuePayloadNow queue $ GLFWCharModsEvent window char modifierKeys

enqueueMouseButtonEvent :: IORef [GLFWEvent] -> GLFW.MouseButtonCallback
enqueueMouseButtonEvent queue window button state modifierKeys =
    enqueuePayloadNow queue $ GLFWMouseButtonEvent window button state modifierKeys

enqueueCursorPosEvent :: IORef [GLFWEvent] -> GLFW.CursorPosCallback
enqueueCursorPosEvent queue window x y =
    enqueuePayloadNow queue $ GLFWCursorPosEvent window x y

enqueueCursorEnterEvent :: IORef [GLFWEvent] -> GLFW.CursorEnterCallback
enqueueCursorEnterEvent queue window state =
    enqueuePayloadNow queue $ GLFWCursorEnterEvent window state

enqueueScrollEvent :: IORef [GLFWEvent] -> GLFW.ScrollCallback
enqueueScrollEvent queue window x y =
    enqueuePayloadNow queue $ GLFWScrollEvent window x y

enqueueDropEvent :: IORef [GLFWEvent] -> GLFW.DropCallback
enqueueDropEvent queue window paths =
    enqueuePayloadNow queue $ GLFWDropEvent window paths

enqueueJoystickEvent :: IORef [GLFWEvent] -> GLFW.JoystickCallback
enqueueJoystickEvent queue joystick state =
    enqueuePayloadNow queue $ GLFWJoystickEvent joystick state

setCallbacks :: IORef [GLFWEvent] -> GLFW.Window -> IO ()
setCallbacks q w = do
    GLFW.setMonitorCallback (Just (enqueueMonitorEvent q))
    GLFW.setWindowPosCallback w (Just (enqueueWindowPosEvent q))
    GLFW.setWindowSizeCallback w (Just (enqueueWindowSizeEvent q))
    GLFW.setWindowCloseCallback w (Just (enqueueWindowCloseEvent q))
    GLFW.setWindowRefreshCallback w (Just (enqueueWindowRefreshEvent q))
    GLFW.setWindowFocusCallback w (Just (enqueueWindowFocusEvent q))
    GLFW.setWindowIconifyCallback w (Just (enqueueWindowIconifyEvent q))
    GLFW.setFramebufferSizeCallback w (Just (enqueueFramebufferSizeEvent q))
    GLFW.setWindowContentScaleCallback w (Just (enqueueWindowContentScaleEvent q))
    GLFW.setWindowMaximizeCallback w (Just (enqueueWindowMaximizeEvent q))
    GLFW.setKeyCallback w (Just (enqueueKeyEvent q))
    GLFW.setCharCallback w (Just (enqueueCharEvent q))
    GLFW.setCharModsCallback w (Just (enqueueCharModsEvent q))
    GLFW.setMouseButtonCallback w (Just (enqueueMouseButtonEvent q))
    GLFW.setCursorPosCallback w (Just (enqueueCursorPosEvent q))
    GLFW.setCursorEnterCallback w (Just (enqueueCursorEnterEvent q))
    GLFW.setScrollCallback w (Just (enqueueScrollEvent q))
    GLFW.setDropCallback w (Just (enqueueDropEvent q))
    GLFW.setJoystickCallback (Just (enqueueJoystickEvent q))
