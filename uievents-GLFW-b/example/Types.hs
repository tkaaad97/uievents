{-# LANGUAGE ExistentialQuantification #-}
module Types
    ( AttribBinding(..)
    , AttribFormat(..)
    , AttribInfo(..)
    , BindBufferSetting(..)
    , BufferSource(..)
    ) where

import Data.ByteString (ByteString)
import Data.Vector.Storable (Vector)
import Foreign.Storable (Storable)

import qualified Graphics.GL as GL

data AttribFormat = AttribFormat
    { attribFormatSize           :: !Int
    , attribFormatDataType       :: !GL.GLuint
    , attribFormatNormalized     :: !Bool
    , attribFormatRelativeOffset :: !Int
    } deriving (Show, Eq)

data AttribInfo = AttribInfo
    { aiAttribName     :: !ByteString
    , aiAttribLocation :: !GL.GLuint
    , aiAttribSize     :: !GL.GLuint
    , aiAttribDataType :: !GL.GLuint
    } deriving (Show, Eq)

data BindBufferSetting = BindBufferSetting
    { bindBufferSettingOffset :: !Int
    , bindBufferSettingStride :: !Int
    } deriving (Show, Eq)

data AttribBinding = AttribBinding
    { attribBindingIndex        :: !Int
    , attribBindingAttribFormat :: !AttribFormat
    } deriving (Show, Eq)

data BufferSource = forall a. Storable a => BufferSource !(Vector a) !GL.GLenum
