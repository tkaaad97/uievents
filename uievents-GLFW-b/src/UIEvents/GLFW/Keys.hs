module UIEvents.GLFW.Keys
    ( keyUnknown
    , keySpace
    , keyApostrophe
    , keyComma
    , keyMinus
    , keyPeriod
    , keySlash
    , key0
    , key1
    , key2
    , key3
    , key4
    , key5
    , key6
    , key7
    , key8
    , key9
    , keySemicolon
    , keyEqual
    , keyA
    , keyB
    , keyC
    , keyD
    , keyE
    , keyF
    , keyG
    , keyH
    , keyI
    , keyJ
    , keyK
    , keyL
    , keyM
    , keyN
    , keyO
    , keyP
    , keyQ
    , keyR
    , keyS
    , keyT
    , keyU
    , keyV
    , keyW
    , keyX
    , keyY
    , keyZ
    , keyLeftBracket
    , keyBackslash
    , keyRightBracket
    , keyGraveAccent
    , keyWorld1
    , keyWorld2
    , keyEscape
    , keyEnter
    , keyTab
    , keyBackspace
    , keyInsert
    , keyDelete
    , keyRight
    , keyLeft
    , keyDown
    , keyUp
    , keyPageUp
    , keyPageDown
    , keyHome
    , keyEnd
    , keyCapsLock
    , keyScrollLock
    , keyNumLock
    , keyPrintScreen
    , keyPause
    , keyF1
    , keyF2
    , keyF3
    , keyF4
    , keyF5
    , keyF6
    , keyF7
    , keyF8
    , keyF9
    , keyF10
    , keyF11
    , keyF12
    , keyF13
    , keyF14
    , keyF15
    , keyF16
    , keyF17
    , keyF18
    , keyF19
    , keyF20
    , keyF21
    , keyF22
    , keyF23
    , keyF24
    , keyF25
    , keyKp0
    , keyKp1
    , keyKp2
    , keyKp3
    , keyKp4
    , keyKp5
    , keyKp6
    , keyKp7
    , keyKp8
    , keyKp9
    , keyKpDecimal
    , keyKpDivide
    , keyKpMultiply
    , keyKpSubtract
    , keyKpAdd
    , keyKpEnter
    , keyKpEqual
    , keyLeftShift
    , keyLeftControl
    , keyLeftAlt
    , keyLeftSuper
    , keyRightShift
    , keyRightControl
    , keyRightAlt
    , keyRightSuper
    , keyMenu
    , keyLast
    ) where

import Bindings.GLFW
import UIEvents (Key(..))

keyUnknown :: Key
keyUnknown = Key c'GLFW_KEY_UNKNOWN

keySpace :: Key
keySpace = Key c'GLFW_KEY_SPACE

keyApostrophe :: Key
keyApostrophe = Key c'GLFW_KEY_APOSTROPHE

keyComma :: Key
keyComma = Key c'GLFW_KEY_COMMA

keyMinus :: Key
keyMinus = Key c'GLFW_KEY_MINUS

keyPeriod :: Key
keyPeriod = Key c'GLFW_KEY_PERIOD

keySlash :: Key
keySlash = Key c'GLFW_KEY_SLASH

key0 :: Key
key0 = Key c'GLFW_KEY_0

key1 :: Key
key1 = Key c'GLFW_KEY_1

key2 :: Key
key2 = Key c'GLFW_KEY_2

key3 :: Key
key3 = Key c'GLFW_KEY_3

key4 :: Key
key4 = Key c'GLFW_KEY_4

key5 :: Key
key5 = Key c'GLFW_KEY_5

key6 :: Key
key6 = Key c'GLFW_KEY_6

key7 :: Key
key7 = Key c'GLFW_KEY_7

key8 :: Key
key8 = Key c'GLFW_KEY_8

key9 :: Key
key9 = Key c'GLFW_KEY_9

keySemicolon :: Key
keySemicolon = Key c'GLFW_KEY_SEMICOLON

keyEqual :: Key
keyEqual = Key c'GLFW_KEY_EQUAL

keyA :: Key
keyA = Key c'GLFW_KEY_A

keyB :: Key
keyB = Key c'GLFW_KEY_B

keyC :: Key
keyC = Key c'GLFW_KEY_C

keyD :: Key
keyD = Key c'GLFW_KEY_D

keyE :: Key
keyE = Key c'GLFW_KEY_E

keyF :: Key
keyF = Key c'GLFW_KEY_F

keyG :: Key
keyG = Key c'GLFW_KEY_G

keyH :: Key
keyH = Key c'GLFW_KEY_H

keyI :: Key
keyI = Key c'GLFW_KEY_I

keyJ :: Key
keyJ = Key c'GLFW_KEY_J

keyK :: Key
keyK = Key c'GLFW_KEY_K

keyL :: Key
keyL = Key c'GLFW_KEY_L

keyM :: Key
keyM = Key c'GLFW_KEY_M

keyN :: Key
keyN = Key c'GLFW_KEY_N

keyO :: Key
keyO = Key c'GLFW_KEY_O

keyP :: Key
keyP = Key c'GLFW_KEY_P

keyQ :: Key
keyQ = Key c'GLFW_KEY_Q

keyR :: Key
keyR = Key c'GLFW_KEY_R

keyS :: Key
keyS = Key c'GLFW_KEY_S

keyT :: Key
keyT = Key c'GLFW_KEY_T

keyU :: Key
keyU = Key c'GLFW_KEY_U

keyV :: Key
keyV = Key c'GLFW_KEY_V

keyW :: Key
keyW = Key c'GLFW_KEY_W

keyX :: Key
keyX = Key c'GLFW_KEY_X

keyY :: Key
keyY = Key c'GLFW_KEY_Y

keyZ :: Key
keyZ = Key c'GLFW_KEY_Z

keyLeftBracket :: Key
keyLeftBracket = Key c'GLFW_KEY_LEFT_BRACKET

keyBackslash :: Key
keyBackslash = Key c'GLFW_KEY_BACKSLASH

keyRightBracket :: Key
keyRightBracket = Key c'GLFW_KEY_RIGHT_BRACKET

keyGraveAccent :: Key
keyGraveAccent = Key c'GLFW_KEY_GRAVE_ACCENT

keyWorld1 :: Key
keyWorld1 = Key c'GLFW_KEY_WORLD_1

keyWorld2 :: Key
keyWorld2 = Key c'GLFW_KEY_WORLD_2

keyEscape :: Key
keyEscape = Key c'GLFW_KEY_ESCAPE

keyEnter :: Key
keyEnter = Key c'GLFW_KEY_ENTER

keyTab :: Key
keyTab = Key c'GLFW_KEY_TAB

keyBackspace :: Key
keyBackspace = Key c'GLFW_KEY_BACKSPACE

keyInsert :: Key
keyInsert = Key c'GLFW_KEY_INSERT

keyDelete :: Key
keyDelete = Key c'GLFW_KEY_DELETE

keyRight :: Key
keyRight = Key c'GLFW_KEY_RIGHT

keyLeft :: Key
keyLeft = Key c'GLFW_KEY_LEFT

keyDown :: Key
keyDown = Key c'GLFW_KEY_DOWN

keyUp :: Key
keyUp = Key c'GLFW_KEY_UP

keyPageUp :: Key
keyPageUp = Key c'GLFW_KEY_PAGE_UP

keyPageDown :: Key
keyPageDown = Key c'GLFW_KEY_PAGE_DOWN

keyHome :: Key
keyHome = Key c'GLFW_KEY_HOME

keyEnd :: Key
keyEnd = Key c'GLFW_KEY_END

keyCapsLock :: Key
keyCapsLock = Key c'GLFW_KEY_CAPS_LOCK

keyScrollLock :: Key
keyScrollLock = Key c'GLFW_KEY_SCROLL_LOCK

keyNumLock :: Key
keyNumLock = Key c'GLFW_KEY_NUM_LOCK

keyPrintScreen :: Key
keyPrintScreen = Key c'GLFW_KEY_PRINT_SCREEN

keyPause :: Key
keyPause = Key c'GLFW_KEY_PAUSE

keyF1 :: Key
keyF1 = Key c'GLFW_KEY_F1

keyF2 :: Key
keyF2 = Key c'GLFW_KEY_F2

keyF3 :: Key
keyF3 = Key c'GLFW_KEY_F3

keyF4 :: Key
keyF4 = Key c'GLFW_KEY_F4

keyF5 :: Key
keyF5 = Key c'GLFW_KEY_F5

keyF6 :: Key
keyF6 = Key c'GLFW_KEY_F6

keyF7 :: Key
keyF7 = Key c'GLFW_KEY_F7

keyF8 :: Key
keyF8 = Key c'GLFW_KEY_F8

keyF9 :: Key
keyF9 = Key c'GLFW_KEY_F9

keyF10 :: Key
keyF10 = Key c'GLFW_KEY_F10

keyF11 :: Key
keyF11 = Key c'GLFW_KEY_F11

keyF12 :: Key
keyF12 = Key c'GLFW_KEY_F12

keyF13 :: Key
keyF13 = Key c'GLFW_KEY_F13

keyF14 :: Key
keyF14 = Key c'GLFW_KEY_F14

keyF15 :: Key
keyF15 = Key c'GLFW_KEY_F15

keyF16 :: Key
keyF16 = Key c'GLFW_KEY_F16

keyF17 :: Key
keyF17 = Key c'GLFW_KEY_F17

keyF18 :: Key
keyF18 = Key c'GLFW_KEY_F18

keyF19 :: Key
keyF19 = Key c'GLFW_KEY_F19

keyF20 :: Key
keyF20 = Key c'GLFW_KEY_F20

keyF21 :: Key
keyF21 = Key c'GLFW_KEY_F21

keyF22 :: Key
keyF22 = Key c'GLFW_KEY_F22

keyF23 :: Key
keyF23 = Key c'GLFW_KEY_F23

keyF24 :: Key
keyF24 = Key c'GLFW_KEY_F24

keyF25 :: Key
keyF25 = Key c'GLFW_KEY_F25

keyKp0 :: Key
keyKp0 = Key c'GLFW_KEY_KP_0

keyKp1 :: Key
keyKp1 = Key c'GLFW_KEY_KP_1

keyKp2 :: Key
keyKp2 = Key c'GLFW_KEY_KP_2

keyKp3 :: Key
keyKp3 = Key c'GLFW_KEY_KP_3

keyKp4 :: Key
keyKp4 = Key c'GLFW_KEY_KP_4

keyKp5 :: Key
keyKp5 = Key c'GLFW_KEY_KP_5

keyKp6 :: Key
keyKp6 = Key c'GLFW_KEY_KP_6

keyKp7 :: Key
keyKp7 = Key c'GLFW_KEY_KP_7

keyKp8 :: Key
keyKp8 = Key c'GLFW_KEY_KP_8

keyKp9 :: Key
keyKp9 = Key c'GLFW_KEY_KP_9

keyKpDecimal :: Key
keyKpDecimal = Key c'GLFW_KEY_KP_DECIMAL

keyKpDivide :: Key
keyKpDivide = Key c'GLFW_KEY_KP_DIVIDE

keyKpMultiply :: Key
keyKpMultiply = Key c'GLFW_KEY_KP_MULTIPLY

keyKpSubtract :: Key
keyKpSubtract = Key c'GLFW_KEY_KP_SUBTRACT

keyKpAdd :: Key
keyKpAdd = Key c'GLFW_KEY_KP_ADD

keyKpEnter :: Key
keyKpEnter = Key c'GLFW_KEY_KP_ENTER

keyKpEqual :: Key
keyKpEqual = Key c'GLFW_KEY_KP_EQUAL

keyLeftShift :: Key
keyLeftShift = Key c'GLFW_KEY_LEFT_SHIFT

keyLeftControl :: Key
keyLeftControl = Key c'GLFW_KEY_LEFT_CONTROL

keyLeftAlt :: Key
keyLeftAlt = Key c'GLFW_KEY_LEFT_ALT

keyLeftSuper :: Key
keyLeftSuper = Key c'GLFW_KEY_LEFT_SUPER

keyRightShift :: Key
keyRightShift = Key c'GLFW_KEY_RIGHT_SHIFT

keyRightControl :: Key
keyRightControl = Key c'GLFW_KEY_RIGHT_CONTROL

keyRightAlt :: Key
keyRightAlt = Key c'GLFW_KEY_RIGHT_ALT

keyRightSuper :: Key
keyRightSuper = Key c'GLFW_KEY_RIGHT_SUPER

keyMenu :: Key
keyMenu = Key c'GLFW_KEY_MENU

keyLast :: Key
keyLast = Key c'GLFW_KEY_LAST
