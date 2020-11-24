{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (useAsCString, useAsCStringLen)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (elems, fromList, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.String.QQ (s)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector (fromList, head, length,
                                                 unsafeWith)
import Example (createUI)
import qualified Foreign (Storable(..), alloca, castPtr, plusPtr, with)
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear (V3(..), V4(..))
import System.Exit (exitSuccess)
import Types

data Handle = Handle !GL.GLuint ![GL.GLuint] !GL.GLuint !GL.GLuint

data Vertex = Vertex (V3 GL.GLfloat) (V4 GL.GLfloat)
    deriving (Show, Eq)

instance Foreign.Storable Vertex where
    sizeOf _ = 28
    alignment _ = 4
    peek ptr = do
        p <- Foreign.peek $ Foreign.castPtr ptr
        c <- Foreign.peek $ Foreign.castPtr ptr `Foreign.plusPtr` Foreign.sizeOf p
        return $ Vertex p c

    poke ptr (Vertex p c) = do
        Foreign.poke (Foreign.castPtr ptr) p
        Foreign.poke (Foreign.castPtr ptr `Foreign.plusPtr` Foreign.sizeOf p) c

main :: IO ()
main = withWindow "uievent-GLFW-b:example" (truncate windowWidth, truncate windowHeight) $ \w -> do
    _ <- createUI windowWidth windowHeight
    return ()
    where
    windowWidth = 1200
    windowHeight = 800

    vertices :: Vector Vertex
    vertices = Vector.fromList
        [ Vertex (V3 0 0 0) (V4 1 0 0 1)
        , Vertex (V3 1 1 0) (V4 0 1 0 1)
        , Vertex (V3 0 1 0) (V4 0 0 1 1)
        , Vertex (V3 1 1 0) (V4 0 1 0 1)
        , Vertex (V3 0 0 0) (V4 1 0 0 1)
        , Vertex (V3 1 0 0) (V4 1 1 1 1)
        ]

    attribBindings = Map.fromList
        [("position", AttribBinding 0 (AttribFormat 3 GL.GL_FLOAT False 0))
        ,("color", AttribBinding 0 (AttribFormat 4 GL.GL_FLOAT False 12))
        ]

    bufferSources = IntMap.fromList
        [(0, (BufferSource vertices GL.GL_STATIC_READ, BindBufferSetting 0 (Foreign.sizeOf (undefined :: Vertex))))]

    mkBufferBinding (source, setting) = do
        buffer <- mkBuffer source
        return (buffer, setting)


    initialize = do
        program <- mkProgram
        bufferBindings <- mapM mkBufferBinding bufferSources
        let buffers = map fst . IntMap.elems $ bufferBindings
        va <- mkVertexArray attribBindings bufferBindings Nothing program
        ul <- getUniformLocation program "projectionViewMatrix"
        return (Handle program buffers va ul)

withWindow :: String -> (Int, Int) -> (GLFW.Window -> IO ()) -> IO ()
withWindow title (width, height) f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 5
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 32)
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              GLFW.setWindowSizeCallback win (Just resizeWindow)
              GLFW.setWindowCloseCallback win (Just shutdown)
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
    GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)

mkProgram :: IO GL.GLuint
mkProgram = do
    program <- GL.glCreateProgram
    vertexShader <- mkVertexShader
    fragmentShader <- mkFragmentShader
    GL.glAttachShader program vertexShader
    GL.glAttachShader program fragmentShader
    GL.glLinkProgram program
    GL.glDeleteShader vertexShader
    GL.glDeleteShader fragmentShader
    return program

mkShader :: GL.GLenum -> ByteString -> IO GL.GLuint
mkShader shaderType source =
    BS.useAsCStringLen source $ \(source', len) ->
    Foreign.with source' $ \sp ->
    Foreign.with (fromIntegral len) $ \lp -> do
        shader <- GL.glCreateShader shaderType
        GL.glShaderSource shader 1 sp lp
        GL.glCompileShader shader
        return shader

vertexShaderSource :: ByteString
vertexShaderSource = [s|
#version 450

in vec3 position;
in vec4 color;

out vec4 fragmentColor;

uniform mat4 projectionViewMatrix = mat4(1.0);
uniform mat4 modelMatrix = mat4(1.0);

void main()
{
    gl_Position = projectionViewMatrix * modelMatrix * vec4(position, 1.0);
    fragmentColor = color;
}
|]

fragmentShaderSource :: ByteString
fragmentShaderSource = [s|
#version 450

in vec4 fragmentColor;
out vec4 outColor;

void main()
{
    outColor = fragmentColor;
}
|]

mkVertexShader :: IO GL.GLuint
mkVertexShader = mkShader GL.GL_VERTEX_SHADER vertexShaderSource

mkFragmentShader :: IO GL.GLuint
mkFragmentShader = mkShader GL.GL_FRAGMENT_SHADER fragmentShaderSource

mkBuffer :: BufferSource -> IO GL.GLuint
mkBuffer (BufferSource vec usage) = do
    let n = Vector.length vec
        size = fromIntegral $ n * Foreign.sizeOf (Vector.head vec)
    buffer <- Foreign.alloca $ \p -> do
        GL.glCreateBuffers 1 p
        Foreign.peek p
    Vector.unsafeWith vec $ \p -> GL.glNamedBufferData buffer size (Foreign.castPtr p) usage
    return buffer

mkVertexArray :: Map ByteString AttribBinding -> IntMap (GL.GLuint, BindBufferSetting) -> Maybe GL.GLuint -> GL.GLuint -> IO GL.GLuint
mkVertexArray attribBindings buffers indexBuffer program = do
    va <- Foreign.alloca $ \p -> do
        GL.glCreateVertexArrays 1 p
        Foreign.peek p
    GL.glUseProgram program
    mapM_ (setAttrib va) (Map.toList attribBindings)
    mapM_ (setBindingBuffer va) . IntMap.toList $ buffers
    maybe (return ()) (GL.glVertexArrayElementBuffer va) indexBuffer
    GL.glUseProgram 0
    return va

    where
    setAttrib vao (k, a) = do
        location <- getAttribLocation program k
        setVertexArrayAttribFormatAndBinding vao location a

    setBindingBuffer vao (i, (b, BindBufferSetting offset stride)) =
        GL.glVertexArrayVertexBuffer vao (fromIntegral i) b (fromIntegral offset) (fromIntegral stride)

setVertexArrayAttribFormatAndBinding :: GL.GLuint -> GL.GLuint -> AttribBinding -> IO ()
setVertexArrayAttribFormatAndBinding vao attribLocation (AttribBinding binding' format) = do
    GL.glVertexArrayAttribBinding vao attribLocation binding
    GL.glVertexArrayAttribFormat vao attribLocation formatSize formatDataType formatNormalized formatRelativeOffset
    GL.glEnableVertexArrayAttrib vao attribLocation

    where
    binding = fromIntegral binding'
    AttribFormat fsize formatDataType fnormalized foffset = format
    formatSize = fromIntegral fsize
    formatNormalized = fromIntegral . fromEnum $ fnormalized
    formatRelativeOffset = fromIntegral foffset

getAttribLocation :: GL.GLuint -> ByteString -> IO GL.GLuint
getAttribLocation program attribName =
    BS.useAsCString attribName $ \a -> do
        r <- GL.glGetAttribLocation program a
        unless (r >= 0) $ throwIO . userError $ "failed to getAttribLocation"
        return (fromIntegral r)

getUniformLocation :: GL.GLuint -> ByteString -> IO GL.GLuint
getUniformLocation program uniformName =
    BS.useAsCString uniformName $ \a -> do
        r <- GL.glGetUniformLocation program a
        unless (r >= 0) $ throwIO . userError $ "failed to getUniformLocation"
        return (fromIntegral r)

