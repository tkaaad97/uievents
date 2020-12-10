{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (useAsCString, useAsCStringLen)
import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (elems, fromList, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.String.QQ (s)
import qualified Data.Vector.Storable as Vector (freeze, head, length,
                                                 unsafeWith)
import qualified Data.Vector.Storable.Mutable as MVector (IOVector, grow,
                                                          length, new,
                                                          unsafeWith, write)
import Data.Word (Word8)
import Example (createUI)
import qualified Foreign (Storable(..), alloca, castPtr, plusPtr, with)
import qualified Graphics.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Linear (M44, V2(..), V3(..), V4(..))
import qualified Linear (ortho)
import System.Exit (exitSuccess)
import Types
import qualified UIEvents (DispatchResult(..), Location(..), UIElement(..),
                           UIEntity(..), UIEventDispatcher, foldUIEntities)
import qualified UIEvents.GLFW as UIEvents (EventQueue, pollEventsDispatch,
                                            setCallbacks)

data RenderInfo = RenderInfo
    { riProgram       :: !GL.GLuint
    , riPrimitiveType :: !GL.GLenum
    , riVertexArray   :: !GL.GLuint
    , riVertexBuffer  :: !GL.GLuint
    , riVertexVector  :: !(MVector.IOVector Vertex, Int)
    , riPVMat         :: !(GL.GLint, M44 Float)
    }

data App = App
    { appMeshRenderInfo    :: !RenderInfo
    , appOutlineRenderInfo :: !RenderInfo
    , appEventQueue        :: !UIEvents.EventQueue
    , appEventDispatcher   :: !(UIEvents.UIEventDispatcher (V4 Word8))
    }

data Vertex = Vertex !(V3 GL.GLfloat) !(V4 Word8)
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
main = withWindow "uievent-GLFW-b:example" (truncate windowWidth, truncate windowHeight) $ \w -> loop w =<< initialize w
    where
    windowWidth = 1200
    windowHeight = 800

    initialize window = do
        d <- createUI windowWidth windowHeight
        program <- mkProgram
        meshRenderInfo <- mkMeshRenderInfo d (windowWidth, windowHeight) program
        outlineRenderInfo <- mkOutlineRenderInfo d (windowWidth, windowHeight) program
        GL.glViewport 0 0 (floor windowWidth) (floor windowHeight)
        GL.glDisable GL.GL_CULL_FACE
        GL.glEnable GL.GL_DEPTH_TEST
        GL.glDepthMask GL.GL_TRUE
        GL.glDepthFunc GL.GL_LESS
        q <- UIEvents.setCallbacks window
        return (App meshRenderInfo outlineRenderInfo q d)

    loop window (App mesh outline q d) = do
        meshVec <- mkVertexVector d (fst . riVertexVector $ mesh)
        outlineVec <- mkOutlineVertexVector d (fst . riVertexVector $ outline)
        let mesh' = mesh { riVertexVector = meshVec }
            outline' = outline { riVertexVector = outlineVec }
        renderStart
        render outline'
        render mesh'
        GLFW.swapBuffers window
        GLFW.pollEvents
        threadDelay . round $ (1E+6 / 60 :: Double)
        r <- UIEvents.pollEventsDispatch q d
        case r of
            UIEvents.DispatchContinue -> loop window (App mesh' outline' q d)
            UIEvents.DispatchExit     -> return ()

    renderStart = do
        GL.glClearColor 1 1 1 1
        GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    render (RenderInfo program primitive vertexArray vertexBuffer (vec, len) (uniformLocation, pvm)) = do
        let byteSize = len * Foreign.sizeOf (undefined :: Vertex)
        MVector.unsafeWith vec $ \ptr -> GL.glNamedBufferData vertexBuffer (fromIntegral byteSize) (Foreign.castPtr ptr) GL.GL_DYNAMIC_DRAW
        GL.glUseProgram program
        Foreign.with pvm $
            GL.glUniformMatrix4fv uniformLocation 1 GL.GL_TRUE . coerce
        GL.glBindVertexArray vertexArray
        GL.glDrawArrays primitive 0 (fromIntegral len)
        GL.glBindVertexArray 0
        GL.glUseProgram 0

mkMeshRenderInfo :: UIEvents.UIEventDispatcher (V4 Word8) -> (Double, Double) -> GL.GLuint -> IO RenderInfo
mkMeshRenderInfo dispatcher (windowWidth, windowHeight) program = do
    (v, len) <- mkVertexVector dispatcher =<< MVector.new 200
    vertices <- Vector.freeze v
    let bufferSources = IntMap.fromList
            [(0, (BufferSource vertices GL.GL_DYNAMIC_DRAW, BindBufferSetting 0 (Foreign.sizeOf (undefined :: Vertex))))]
    bufferBindings <- mapM mkBufferBinding bufferSources
    let buffer = head . map fst . IntMap.elems $ bufferBindings
    GL.glUseProgram program
    va <- mkVertexArray attribBindings bufferBindings Nothing program
    ul <- getUniformLocation program "projectionViewMatrix"
    let pvm = Linear.ortho 0 (realToFrac windowWidth) (realToFrac windowHeight) 0 (-1) 1
    GL.glUseProgram 0
    return (RenderInfo program GL.GL_TRIANGLES va buffer (v, len) (fromIntegral ul, pvm))
    where
    attribBindings = Map.fromList
        [("position", AttribBinding 0 (AttribFormat 3 GL.GL_FLOAT False 0))
        ,("color", AttribBinding 0 (AttribFormat 4 GL.GL_UNSIGNED_BYTE True 12))
        ]
    mkBufferBinding (source, setting) = do
        buffer <- mkBuffer source
        return (buffer, setting)

mkOutlineRenderInfo :: UIEvents.UIEventDispatcher (V4 Word8) -> (Double, Double) -> GL.GLuint -> IO RenderInfo
mkOutlineRenderInfo dispatcher (windowWidth, windowHeight) program = do
    (v, len) <- mkOutlineVertexVector dispatcher =<< MVector.new 200
    vertices <- Vector.freeze v
    let bufferSources = IntMap.fromList
            [(0, (BufferSource vertices GL.GL_DYNAMIC_DRAW, BindBufferSetting 0 (Foreign.sizeOf (undefined :: Vertex))))]
    bufferBindings <- mapM mkBufferBinding bufferSources
    let buffer = head . map fst . IntMap.elems $ bufferBindings
    GL.glUseProgram program
    va <- mkVertexArray attribBindings bufferBindings Nothing program
    ul <- getUniformLocation program "projectionViewMatrix"
    let pvm = Linear.ortho 0 (realToFrac windowWidth) (realToFrac windowHeight) 0 (-1) 1
    GL.glUseProgram 0
    return (RenderInfo program GL.GL_LINES va buffer (v, len) (fromIntegral ul, pvm))
    where
    attribBindings = Map.fromList
        [("position", AttribBinding 0 (AttribFormat 3 GL.GL_FLOAT False 0))
        ,("color", AttribBinding 0 (AttribFormat 4 GL.GL_UNSIGNED_BYTE True 12))
        ]
    mkBufferBinding (source, setting) = do
        buffer <- mkBuffer source
        return (buffer, setting)

mkVertexVector :: UIEvents.UIEventDispatcher (V4 Word8) -> MVector.IOVector Vertex -> IO (MVector.IOVector Vertex, Int)
mkVertexVector d vec0 = do
    (vec1, len, _) <- UIEvents.foldUIEntities d go (True, V2 0 0) (vec0, 0, 0)
    return (vec1, len)
    where
    deltaZ = 1 / 256
    go entity (True, p0) (vec, len, z)
        | not $ UIEvents.uielementDisplay (UIEvents.uientityElement entity) = return ((False, p0), (vec, len, z))
        | len + 6 < MVector.length vec = do
            let element = UIEvents.uientityElement entity
                color = UIEvents.uielementValue element
                UIEvents.Location p1 size = UIEvents.uielementLocation element
                V2 px py = fmap realToFrac $ p0 + p1
                V2 sx sy = fmap realToFrac size
                vertices =
                    [ (len,     Vertex (V3 px py z) color)
                    , (len + 1, Vertex (V3 (px + sx) (py + sy) z) color)
                    , (len + 2, Vertex (V3 (px + sx) py z) color)
                    , (len + 3, Vertex (V3 (px + sx) (py + sy) z) color)
                    , (len + 4, Vertex (V3 px py z) color)
                    , (len + 5, Vertex (V3 px (py + sy) z) color)
                    ]
            mapM_ (uncurry $ MVector.write vec) vertices
            return ((True, p1), (vec, len + 6, z + deltaZ))
        | otherwise = do
            v' <- MVector.grow vec (MVector.length vec)
            go entity (True, p0) (v', MVector.length v', z)
    go _ a b = return (a, b)

mkOutlineVertexVector :: UIEvents.UIEventDispatcher (V4 Word8) -> MVector.IOVector Vertex -> IO (MVector.IOVector Vertex, Int)
mkOutlineVertexVector d vec0 = do
    (vec1, len, _) <- UIEvents.foldUIEntities d go (True, V2 0 0) (vec0, 0, deltaZ / 2)
    return (vec1, len)
    where
    deltaZ = 1 / 256
    go entity (True, p0) (vec, len, z)
        | not $ UIEvents.uielementDisplay (UIEvents.uientityElement entity) = return ((False, p0), (vec, len, z))
        | len + 8 < MVector.length vec = do
            let element = UIEvents.uientityElement entity
                lineColor = V4 32 32 32 255
                UIEvents.Location p1 size = UIEvents.uielementLocation element
                V2 px py = fmap realToFrac $ p0 + p1
                V2 sx sy = fmap realToFrac size
                vertices =
                    [ (len,     Vertex (V3 px py z) lineColor)
                    , (len + 1, Vertex (V3 px (py + sy) z) lineColor)
                    , (len + 2, Vertex (V3 px (py + sy) z) lineColor)
                    , (len + 3, Vertex (V3 (px + sx) (py + sy) z) lineColor)
                    , (len + 4, Vertex (V3 (px + sx) (py + sy) z) lineColor)
                    , (len + 5, Vertex (V3 (px + sx) py z) lineColor)
                    , (len + 6, Vertex (V3 (px + sx) py z) lineColor)
                    , (len + 7, Vertex (V3 px py z) lineColor)
                    ]
            mapM_ (uncurry $ MVector.write vec) vertices
            return ((True, p1), (vec, len + 8, z + deltaZ))
        | otherwise = do
            v' <- MVector.grow vec (MVector.length vec)
            go entity (True, p0) (v', MVector.length v', z)
    go _ a b = return (a, b)

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
    simpleErrorCallback e m =
        putStrLn $ unwords [show e, show m]

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

