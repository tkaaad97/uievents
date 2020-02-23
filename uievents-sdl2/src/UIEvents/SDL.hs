module UIEvents.SDL
    (
    ) where

import qualified SDL (Event(..), EventPayload(..), InputMotion(..),
                      KeyboardEventData(..), Keycode(..), Keysym(..),
                      MouseButton(..), MouseButtonEventData(..),
                      MouseMotionEventData(..), Point(..), Timestamp,
                      WindowResizedEventData(..))
import qualified UIEvents

convertEvent :: SDL.Event -> Maybe UIEvents.UIEvent
convertEvent (SDL.Event timestamp payload) = do
    payload' <- convertPayload payload
    let timestamp' = convertTimestamp timestamp
    return $ UIEvents.UIEvent timestamp' payload'

convertTimestamp :: SDL.Timestamp -> UIEvents.Timestamp
convertTimestamp timestamp = UIEvents.Timestamp (1000 * fromIntegral timestamp)

convertPayload :: SDL.EventPayload -> Maybe UIEvents.UIEventPayload
convertPayload (SDL.WindowResizedEvent payload) = Just $ UIEvents.WindowResizeEvent' (UIEvents.WindowResizeEvent (SDL.windowResizedEventSize payload))
convertPayload (SDL.WindowClosedEvent _) = Just $ UIEvents.WindowCloseEvent' UIEvents.WindowCloseEvent
convertPayload (SDL.MouseMotionEvent payload) =
    let SDL.P position = SDL.mouseMotionEventPos payload
        movement = SDL.mouseMotionEventRelMotion payload
        buttons = map convertMouseButton . SDL.mouseMotionEventState $ payload
        payload' = UIEvents.MouseMotionEvent
            { UIEvents.mouseMotionEventPosition = position
            , UIEvents.mouseMotionEventMovement = movement
            , UIEvents.mouseMotionEventButtons  = buttons
            }
    in Just $ UIEvents.MouseMotionEvent' payload'
convertPayload (SDL.MouseButtonEvent payload) =
    let SDL.P position = SDL.mouseButtonEventPos payload
        button = convertMouseButton . SDL.mouseButtonEventButton $ payload
        eventType = case SDL.mouseButtonEventMotion payload of
            SDL.Released -> UIEvents.MouseButtonReleased
            SDL.Pressed  -> UIEvents.MouseButtonPressed
        payload' = UIEvents.MouseButtonEvent
            { UIEvents.mouseButtonEventType = eventType
            , UIEvents.mouseButtonEventButton = button
            , UIEvents.mouseButtonEventPosition = position
            }
    in Just . UIEvents.MouseButtonEvent' $ payload'
convertPayload (SDL.KeyboardEvent payload) =
    let eventType = case (SDL.keyboardEventKeyMotion payload, SDL.keyboardEventRepeat payload) of
            (SDL.Pressed, False) -> UIEvents.KeyPressed
            (SDL.Pressed, True)  -> UIEvents.KeyRepeated
            (SDL.Released, _)    -> UIEvents.KeyReleased
        key = UIEvents.Keycode . SDL.unwrapKeycode . SDL.keysymKeycode . SDL.keyboardEventKeysym $ payload
        payload' = UIEvents.KeyboardEvent
            { UIEvents.keyboardEventType = eventType
            , UIEvents.keyboardEventKey = key
            }
    in Just . UIEvents.KeyboardEvent' $ payload'
convertPayload _ = Nothing

convertMouseButton :: SDL.MouseButton -> UIEvents.MouseButton
convertMouseButton SDL.ButtonLeft           = UIEvents.MouseButtonLeft
convertMouseButton SDL.ButtonMiddle         = UIEvents.MouseButtonMiddle
convertMouseButton SDL.ButtonRight          = UIEvents.MouseButtonRight
convertMouseButton SDL.ButtonX1             = UIEvents.MouseButtonExtra 3
convertMouseButton SDL.ButtonX2             = UIEvents.MouseButtonExtra 4
convertMouseButton (SDL.ButtonExtra button) = UIEvents.MouseButtonExtra (fromIntegral button)
