module UIEvents.GLFW
    (
    ) where

import qualified Chronos as Time (Time(..), now)
import Control.Monad (filterM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2(..))
import qualified UIEvents

fromGLFWMouseButton :: GLFW.MouseButton -> UIEvents.MouseButton
fromGLFWMouseButton GLFW.MouseButton'1 = UIEvents.MouseButtonLeft
fromGLFWMouseButton GLFW.MouseButton'2 = UIEvents.MouseButtonRight
fromGLFWMouseButton GLFW.MouseButton'3 = UIEvents.MouseButtonMiddle
fromGLFWMouseButton GLFW.MouseButton'4 = UIEvents.MouseButtonExtra 3
fromGLFWMouseButton GLFW.MouseButton'5 = UIEvents.MouseButtonExtra 4
fromGLFWMouseButton GLFW.MouseButton'6 = UIEvents.MouseButtonExtra 5
fromGLFWMouseButton GLFW.MouseButton'7 = UIEvents.MouseButtonExtra 6
fromGLFWMouseButton GLFW.MouseButton'8 = UIEvents.MouseButtonExtra 7

fromGLFWMouseButtonState :: GLFW.MouseButtonState -> UIEvents.MouseButtonEventType
fromGLFWMouseButtonState GLFW.MouseButtonState'Pressed = UIEvents.MouseButtonPressed
fromGLFWMouseButtonState GLFW.MouseButtonState'Released = UIEvents.MouseButtonReleased

fromGLFWKeyState :: GLFW.KeyState -> UIEvents.KeyboardEventType
fromGLFWKeyState GLFW.KeyState'Pressed   = UIEvents.KeyPressed
fromGLFWKeyState GLFW.KeyState'Released  = UIEvents.KeyReleased
fromGLFWKeyState GLFW.KeyState'Repeating = UIEvents.KeyRepeated

enqueueEvent :: UIEvents.UIEvent -> [UIEvents.UIEvent] -> ([UIEvents.UIEvent], ())
enqueueEvent event queue = (event : queue, ())

enqueuePayloadNow :: IORef [UIEvents.UIEvent] -> UIEvents.UIEventPayload -> IO ()
enqueuePayloadNow queue payload = do
    t <- UIEvents.Timestamp . Time.getTime <$> Time.now
    let event = UIEvents.UIEvent t payload
    atomicModifyIORef' queue (enqueueEvent event)

enqueueWindowSizeEvent :: IORef [UIEvents.UIEvent] -> GLFW.WindowSizeCallback
enqueueWindowSizeEvent queue _ w h =
    enqueuePayloadNow queue . UIEvents.WindowResizeEvent' . UIEvents.WindowResizeEvent $ V2 (fromIntegral w) (fromIntegral h)

enqueueWindowCloseEvent :: IORef [UIEvents.UIEvent] -> GLFW.WindowCloseCallback
enqueueWindowCloseEvent queue _ =
    enqueuePayloadNow queue UIEvents.WindowCloseEvent'

enqueueWindowFocusEvent :: IORef [UIEvents.UIEvent] -> GLFW.WindowFocusCallback
enqueueWindowFocusEvent queue _ focus
    | focus = enqueuePayloadNow queue UIEvents.WindowEnterEvent'
    | otherwise = enqueuePayloadNow queue UIEvents.WindowLeaveEvent'

enqueueKeyEvent :: IORef [UIEvents.UIEvent] -> GLFW.KeyCallback
enqueueKeyEvent queue _ key scan keyState _ = do
    scan <- GLFW.getKeyScancode key
    enqueuePayloadNow queue . UIEvents.KeyboardEvent' $ UIEvents.KeyboardEvent (fromGLFWKeyState keyState) (UIEvents.Key (fromIntegral scan))

enqueueMouseButtonEvent :: IORef [UIEvents.UIEvent] -> GLFW.MouseButtonCallback
enqueueMouseButtonEvent queue window button state _ = do
    (x, y) <- GLFW.getCursorPos window
    let pos = V2 (round x) (round y)
    enqueuePayloadNow queue . UIEvents.MouseButtonEvent' $ UIEvents.MouseButtonEvent eventType mouseButton pos
    where
    eventType = fromGLFWMouseButtonState state
    mouseButton = fromGLFWMouseButton button

enqueueCursorPosEvent :: IORef [UIEvents.UIEvent] -> IORef (Double, Double) -> GLFW.CursorPosCallback
enqueueCursorPosEvent queue posRef window x y = do
    (px, py) <- readIORef posRef
    let delta = V2 (round (x - px)) (round (y - py))
    buttons <- filterM (fmap (== GLFW.MouseButtonState'Pressed) . GLFW.getMouseButton window) [GLFW.MouseButton'1, GLFW.MouseButton'2, GLFW.MouseButton'3]
    enqueuePayloadNow queue . UIEvents.MouseMotionEvent' $ UIEvents.MouseMotionEvent (V2 (round x) (round y)) delta (map fromGLFWMouseButton buttons)

{-
enqueueCursorEnterEvent :: IORef [UIEvents.UIEvent] -> GLFW.CursorEnterCallback
enqueueCursorEnterEvent queue window state =
    enqueuePayloadNow queue $ GLFWCursorEnterEvent window state
-}

setCallbacks :: IORef [UIEvents.UIEvent] -> GLFW.Window -> IO ()
setCallbacks q w = do
    GLFW.setWindowSizeCallback w (Just (enqueueWindowSizeEvent q))
    GLFW.setWindowCloseCallback w (Just (enqueueWindowCloseEvent q))
    GLFW.setWindowFocusCallback w (Just (enqueueWindowFocusEvent q))
    GLFW.setKeyCallback w (Just (enqueueKeyEvent q))
    GLFW.setMouseButtonCallback w (Just (enqueueMouseButtonEvent q))
    posRef <- newIORef =<< GLFW.getCursorPos w
    GLFW.setCursorPosCallback w (Just (enqueueCursorPosEvent q posRef))
