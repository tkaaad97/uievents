{-# LANGUAGE PatternSynonyms #-}
module UIEvents.SDL.Keys
    ( pattern Key0
    , pattern Key1
    , pattern Key2
    , pattern Key3
    , pattern Key4
    , pattern Key5
    , pattern Key6
    , pattern Key7
    , pattern Key8
    , pattern Key9
    , pattern KeyA
    , pattern KeyB
    , pattern KeyC
    , pattern KeyD
    , pattern KeyE
    , pattern KeyF
    , pattern KeyG
    , pattern KeyH
    , pattern KeyI
    , pattern KeyJ
    , pattern KeyK
    , pattern KeyL
    , pattern KeyM
    , pattern KeyN
    , pattern KeyO
    , pattern KeyP
    , pattern KeyQ
    , pattern KeyR
    , pattern KeyS
    , pattern KeyT
    , pattern KeyU
    , pattern KeyV
    , pattern KeyW
    , pattern KeyX
    , pattern KeyY
    , pattern KeyZ
    , pattern KeyApostrophe
    , pattern KeyBackslash
    , pattern KeyBackspace
    , pattern KeyCapsLock
    , pattern KeyComma
    , pattern KeyDelete
    , pattern KeyDown
    , pattern KeyEnd
    , pattern KeyEnter
    , pattern KeyEqual
    , pattern KeyEscape
    , pattern KeyF1
    , pattern KeyF2
    , pattern KeyF3
    , pattern KeyF4
    , pattern KeyF5
    , pattern KeyF6
    , pattern KeyF7
    , pattern KeyF8
    , pattern KeyF9
    , pattern KeyF10
    , pattern KeyF11
    , pattern KeyF12
    , pattern KeyGrave
    , pattern KeyHome
    , pattern KeyInsert
    , pattern KeyKP0
    , pattern KeyKP1
    , pattern KeyKP2
    , pattern KeyKP3
    , pattern KeyKP4
    , pattern KeyKP5
    , pattern KeyKP6
    , pattern KeyKP7
    , pattern KeyKP8
    , pattern KeyKP9
    , pattern KeyKPAdd
    , pattern KeyKPDecimal
    , pattern KeyKPEnter
    , pattern KeyKPEqual
    , pattern KeyKPMultiply
    , pattern KeyKPSlash
    , pattern KeyKPSubtract
    , pattern KeyLeft
    , pattern KeyLeftAlt
    , pattern KeyLeftBracket
    , pattern KeyLeftControl
    , pattern KeyLeftShift
    , pattern KeyMenu
    , pattern KeyMinus
    , pattern KeyNumLock
    , pattern KeyPageDown
    , pattern KeyPageUp
    , pattern KeyPause
    , pattern KeyPeriod
    , pattern KeyPrintScreen
    , pattern KeyRight
    , pattern KeyRightAlt
    , pattern KeyRightBracket
    , pattern KeyRightControl
    , pattern KeyRightShift
    , pattern KeyScrollLock
    , pattern KeySemicolon
    , pattern KeySlash
    , pattern KeySpace
    , pattern KeyTab
    , pattern KeyUp
    ) where

import qualified SDL.Raw
import UIEvents (Key(..))


pattern Key0 :: Key
pattern Key0 = Key SDL.Raw.SDLK_0

pattern Key1 :: Key
pattern Key1 = Key SDL.Raw.SDLK_1

pattern Key2 :: Key
pattern Key2 = Key SDL.Raw.SDLK_2

pattern Key3 :: Key
pattern Key3 = Key SDL.Raw.SDLK_3

pattern Key4 :: Key
pattern Key4 = Key SDL.Raw.SDLK_4

pattern Key5 :: Key
pattern Key5 = Key SDL.Raw.SDLK_5

pattern Key6 :: Key
pattern Key6 = Key SDL.Raw.SDLK_6

pattern Key7 :: Key
pattern Key7 = Key SDL.Raw.SDLK_7

pattern Key8 :: Key
pattern Key8 = Key SDL.Raw.SDLK_8

pattern Key9 :: Key
pattern Key9 = Key SDL.Raw.SDLK_9

pattern KeyA :: Key
pattern KeyA = Key SDL.Raw.SDLK_a

pattern KeyB :: Key
pattern KeyB = Key SDL.Raw.SDLK_b

pattern KeyC :: Key
pattern KeyC = Key SDL.Raw.SDLK_c

pattern KeyD :: Key
pattern KeyD = Key SDL.Raw.SDLK_d

pattern KeyE :: Key
pattern KeyE = Key SDL.Raw.SDLK_e

pattern KeyF :: Key
pattern KeyF = Key SDL.Raw.SDLK_f

pattern KeyG :: Key
pattern KeyG = Key SDL.Raw.SDLK_g

pattern KeyH :: Key
pattern KeyH = Key SDL.Raw.SDLK_h

pattern KeyI :: Key
pattern KeyI = Key SDL.Raw.SDLK_i

pattern KeyJ :: Key
pattern KeyJ = Key SDL.Raw.SDLK_j

pattern KeyK :: Key
pattern KeyK = Key SDL.Raw.SDLK_k

pattern KeyL :: Key
pattern KeyL = Key SDL.Raw.SDLK_l

pattern KeyM :: Key
pattern KeyM = Key SDL.Raw.SDLK_m

pattern KeyN :: Key
pattern KeyN = Key SDL.Raw.SDLK_n

pattern KeyO :: Key
pattern KeyO = Key SDL.Raw.SDLK_o

pattern KeyP :: Key
pattern KeyP = Key SDL.Raw.SDLK_p

pattern KeyQ :: Key
pattern KeyQ = Key SDL.Raw.SDLK_q

pattern KeyR :: Key
pattern KeyR = Key SDL.Raw.SDLK_r

pattern KeyS :: Key
pattern KeyS = Key SDL.Raw.SDLK_s

pattern KeyT :: Key
pattern KeyT = Key SDL.Raw.SDLK_t

pattern KeyU :: Key
pattern KeyU = Key SDL.Raw.SDLK_u

pattern KeyV :: Key
pattern KeyV = Key SDL.Raw.SDLK_v

pattern KeyW :: Key
pattern KeyW = Key SDL.Raw.SDLK_w

pattern KeyX :: Key
pattern KeyX = Key SDL.Raw.SDLK_x

pattern KeyY :: Key
pattern KeyY = Key SDL.Raw.SDLK_y

pattern KeyZ :: Key
pattern KeyZ = Key SDL.Raw.SDLK_z

pattern KeyApostrophe :: Key
pattern KeyApostrophe = Key SDL.Raw.SDLK_QUOTE

pattern KeyBackslash :: Key
pattern KeyBackslash = Key SDL.Raw.SDLK_BACKSLASH

pattern KeyBackspace :: Key
pattern KeyBackspace = Key SDL.Raw.SDLK_BACKSPACE

pattern KeyCapsLock :: Key
pattern KeyCapsLock = Key SDL.Raw.SDLK_CAPSLOCK

pattern KeyComma :: Key
pattern KeyComma = Key SDL.Raw.SDLK_COMMA

pattern KeyDelete :: Key
pattern KeyDelete = Key SDL.Raw.SDLK_DELETE

pattern KeyDown :: Key
pattern KeyDown = Key SDL.Raw.SDLK_DOWN

pattern KeyEnd :: Key
pattern KeyEnd = Key SDL.Raw.SDLK_END

pattern KeyEnter :: Key
pattern KeyEnter = Key SDL.Raw.SDLK_RETURN

pattern KeyEqual :: Key
pattern KeyEqual = Key SDL.Raw.SDLK_EQUALS

pattern KeyEscape :: Key
pattern KeyEscape = Key SDL.Raw.SDLK_ESCAPE

pattern KeyF1 :: Key
pattern KeyF1 = Key SDL.Raw.SDLK_F1

pattern KeyF2 :: Key
pattern KeyF2 = Key SDL.Raw.SDLK_F2

pattern KeyF3 :: Key
pattern KeyF3 = Key SDL.Raw.SDLK_F3

pattern KeyF4 :: Key
pattern KeyF4 = Key SDL.Raw.SDLK_F4

pattern KeyF5 :: Key
pattern KeyF5 = Key SDL.Raw.SDLK_F5

pattern KeyF6 :: Key
pattern KeyF6 = Key SDL.Raw.SDLK_F6

pattern KeyF7 :: Key
pattern KeyF7 = Key SDL.Raw.SDLK_F7

pattern KeyF8 :: Key
pattern KeyF8 = Key SDL.Raw.SDLK_F8

pattern KeyF9 :: Key
pattern KeyF9 = Key SDL.Raw.SDLK_F9

pattern KeyF10 :: Key
pattern KeyF10 = Key SDL.Raw.SDLK_F10

pattern KeyF11 :: Key
pattern KeyF11 = Key SDL.Raw.SDLK_F11

pattern KeyF12 :: Key
pattern KeyF12 = Key SDL.Raw.SDLK_F12

pattern KeyGrave :: Key
pattern KeyGrave = Key SDL.Raw.SDLK_BACKQUOTE

pattern KeyHome :: Key
pattern KeyHome = Key SDL.Raw.SDLK_HOME

pattern KeyInsert :: Key
pattern KeyInsert = Key SDL.Raw.SDLK_INSERT

pattern KeyKP0 :: Key
pattern KeyKP0 = Key SDL.Raw.SDLK_KP_0

pattern KeyKP1 :: Key
pattern KeyKP1 = Key SDL.Raw.SDLK_KP_1

pattern KeyKP2 :: Key
pattern KeyKP2 = Key SDL.Raw.SDLK_KP_2

pattern KeyKP3 :: Key
pattern KeyKP3 = Key SDL.Raw.SDLK_KP_3

pattern KeyKP4 :: Key
pattern KeyKP4 = Key SDL.Raw.SDLK_KP_4

pattern KeyKP5 :: Key
pattern KeyKP5 = Key SDL.Raw.SDLK_KP_5

pattern KeyKP6 :: Key
pattern KeyKP6 = Key SDL.Raw.SDLK_KP_6

pattern KeyKP7 :: Key
pattern KeyKP7 = Key SDL.Raw.SDLK_KP_7

pattern KeyKP8 :: Key
pattern KeyKP8 = Key SDL.Raw.SDLK_KP_8

pattern KeyKP9 :: Key
pattern KeyKP9 = Key SDL.Raw.SDLK_KP_9

pattern KeyKPAdd :: Key
pattern KeyKPAdd = Key SDL.Raw.SDLK_KP_PLUS

pattern KeyKPDecimal :: Key
pattern KeyKPDecimal = Key SDL.Raw.SDLK_KP_DECIMAL

pattern KeyKPEnter :: Key
pattern KeyKPEnter = Key SDL.Raw.SDLK_KP_ENTER

pattern KeyKPEqual :: Key
pattern KeyKPEqual = Key SDL.Raw.SDLK_KP_EQUALS

pattern KeyKPMultiply :: Key
pattern KeyKPMultiply = Key SDL.Raw.SDLK_KP_MULTIPLY

pattern KeyKPSlash :: Key
pattern KeyKPSlash = Key SDL.Raw.SDLK_KP_DIVIDE

pattern KeyKPSubtract :: Key
pattern KeyKPSubtract = Key SDL.Raw.SDLK_KP_MINUS

pattern KeyLeft :: Key
pattern KeyLeft = Key SDL.Raw.SDLK_LEFT

pattern KeyLeftAlt :: Key
pattern KeyLeftAlt = Key SDL.Raw.SDLK_LALT

pattern KeyLeftBracket :: Key
pattern KeyLeftBracket = Key SDL.Raw.SDLK_LEFTBRACKET

pattern KeyLeftControl :: Key
pattern KeyLeftControl = Key SDL.Raw.SDLK_LCTRL

pattern KeyLeftShift :: Key
pattern KeyLeftShift = Key SDL.Raw.SDLK_LSHIFT

pattern KeyMenu :: Key
pattern KeyMenu = Key SDL.Raw.SDLK_MENU

pattern KeyMinus :: Key
pattern KeyMinus = Key SDL.Raw.SDLK_MINUS

pattern KeyNumLock :: Key
pattern KeyNumLock = Key SDL.Raw.SDLK_NUMLOCKCLEAR

pattern KeyPageDown :: Key
pattern KeyPageDown = Key SDL.Raw.SDLK_PAGEDOWN

pattern KeyPageUp :: Key
pattern KeyPageUp = Key SDL.Raw.SDLK_PAGEUP

pattern KeyPause :: Key
pattern KeyPause = Key SDL.Raw.SDLK_PAUSE

pattern KeyPeriod :: Key
pattern KeyPeriod = Key SDL.Raw.SDLK_PERIOD

pattern KeyPrintScreen :: Key
pattern KeyPrintScreen = Key SDL.Raw.SDLK_PRINTSCREEN

pattern KeyRight :: Key
pattern KeyRight = Key SDL.Raw.SDLK_RIGHT

pattern KeyRightAlt :: Key
pattern KeyRightAlt = Key SDL.Raw.SDLK_RALT

pattern KeyRightBracket :: Key
pattern KeyRightBracket = Key SDL.Raw.SDLK_RIGHTBRACKET

pattern KeyRightControl :: Key
pattern KeyRightControl = Key SDL.Raw.SDLK_RCTRL

pattern KeyRightShift :: Key
pattern KeyRightShift = Key SDL.Raw.SDLK_RSHIFT

pattern KeyScrollLock :: Key
pattern KeyScrollLock = Key SDL.Raw.SDLK_SCROLLLOCK

pattern KeySemicolon :: Key
pattern KeySemicolon = Key SDL.Raw.SDLK_SEMICOLON

pattern KeySlash :: Key
pattern KeySlash = Key SDL.Raw.SDLK_SLASH

pattern KeySpace :: Key
pattern KeySpace = Key SDL.Raw.SDLK_SPACE

pattern KeyTab :: Key
pattern KeyTab = Key SDL.Raw.SDLK_TAB

pattern KeyUp :: Key
pattern KeyUp = Key SDL.Raw.SDLK_UP
