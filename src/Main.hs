{-# language DeriveFunctor, ScopedTypeVariables, RecordWildCards #-}
module Main where

import Camera (Camera, lookAt, fly, pan, cameraMatrix)

import qualified Graphics.UI.GLFW as GLFW (
  Window,
  init, terminate,
  createWindow, makeContextCurrent, windowShouldClose,
  pollEvents, swapBuffers, getTime, getCursorPos,
  getMouseButton, MouseButtonState(..), MouseButton(..),
  getKey, KeyState(..), Key(..))

import Graphics.GL

import Linear (
  V2(V2), V3(V3), (*^), (^+^), (^-^),
  norm, quadrance,
  M44, perspective, (!*!))

import Streaming.Prelude (
  Stream, Of)
import qualified Streaming.Prelude as S (
  map, for, take, iterate, yield,
  each, toList_, length_)

import Foreign.Storable (
  Storable(..), peek, sizeOf)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal (alloca, with)
import Foreign.Marshal.Array (withArrayLen)
import Data.Bits ((.|.))

import Text.Printf (printf)
import Control.Monad (unless)
import Control.Applicative (liftA2, liftA3)

main :: IO ()
main = do

  _ <- GLFW.init

  Just window <- GLFW.createWindow 600 600 "Voxel Populi" Nothing Nothing

  GLFW.makeContextCurrent (Just window)

  Just time <- GLFW.getTime
  cursorPos <- GLFW.getCursorPos window

  glEnable GL_DEPTH_TEST
  glClearColor 1 1 1 1

  chunk <- createChunk 8 2 ball

  loop window time cursorPos initialCamera chunk

  deleteChunk chunk

  GLFW.terminate


initialCamera :: Camera
initialCamera = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 1 0)

loop :: GLFW.Window -> Double -> (Double, Double) -> Camera -> Chunk -> IO ()
loop window lastTime (lastCursorX, lastCursorY) camera chunk = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  renderChunk camera chunk

  GLFW.swapBuffers window

  _ <- GLFW.pollEvents

  Just currentTime <- GLFW.getTime

  let frameTime = currentTime - lastTime

  printf "Frametime: %4.3f\n" frameTime

  keystateW <- GLFW.getKey window GLFW.Key'W
  keystateA <- GLFW.getKey window GLFW.Key'A
  keystateS <- GLFW.getKey window GLFW.Key'S
  keystateD <- GLFW.getKey window GLFW.Key'D

  (currentCursorX, currentCursorY) <- GLFW.getCursorPos window
  mouseButtonState <- GLFW.getMouseButton window GLFW.MouseButton'1
  let differenceCursorX = currentCursorX - lastCursorX
      differenceCursorY = currentCursorY - lastCursorY
      mouseScalar = case mouseButtonState of
        GLFW.MouseButtonState'Pressed -> 1
        _ -> 0

  let cameraMovementSpeed = 1
      cameraRotationSpeed = 0.001
      keystateScalar keystate = case keystate of
        GLFW.KeyState'Pressed -> 1
        _ -> 0
      movement = (realToFrac frameTime * cameraMovementSpeed) *^ sum [
        keystateScalar keystateW *^ V3 0 0 (-1),
        keystateScalar keystateA *^ V3 (-1) 0 0,
        keystateScalar keystateS *^ V3 0 0 1,
        keystateScalar keystateD *^ V3 1 0 0]
      rotation = (mouseScalar * cameraRotationSpeed) *^
        fmap realToFrac (V2 differenceCursorX (negate differenceCursorY))

  let camera' = pan rotation (fly movement camera)

  shouldClose <- GLFW.windowShouldClose window

  unless shouldClose (
    loop window currentTime (currentCursorX, currentCursorY) camera' chunk)


type Resolution = Int

data Cube = Cube GLfloat (V3 GLfloat)
  deriving (Show, Eq, Ord)

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

sample :: (Monad m) => Int -> Resolution -> (Cube -> Side) -> Cube -> Stream (Of Cube) m ()
sample depth resolution volume cube
  | depth == 0 = return ()
  | otherwise = S.for (nestedCubes resolution cube) (\cube1 ->
    case volume cube1 of
      Outside -> return ()
      Inside -> S.yield cube1
      Border -> sample (depth - 1) resolution volume cube1)

nestedCubes :: (Monad m) => Resolution -> Cube -> Stream (Of Cube) m ()
nestedCubes resolution outerCube =
  S.map (relative outerCube) (unitCubes resolution)

unitCubes :: (Monad m) => Resolution -> Stream (Of Cube) m ()
unitCubes resolution = do
  let size = recip (realToFrac resolution)
      range = S.take resolution (S.iterate ((+) size) 0)
  S.for range (\x1 ->
    S.for range (\x2 ->
      S.for range (\x3 -> do
        S.yield (Cube size (V3 x1 x2 x3)))))

relative :: Cube -> Cube -> Cube
relative (Cube outerSize outerPosition) (Cube innerSize innerPosition) =
  Cube (outerSize * innerSize) (outerPosition ^+^ (outerSize *^ innerPosition))

ball :: Cube -> Side
ball (Cube size position)
  | distance < circleRadius - cubeRadius = Inside
  | distance > circleRadius + cubeRadius = Outside
  | otherwise = Border where
    circleCenter = V3 0.5 0.5 0.5
    circleRadius = 0.5
    cubeCenter = position ^+^ halfSize
    cubeRadius = norm halfSize
    halfSize = 0.5 *^ (V3 size size size)
    distance = norm (circleCenter ^-^ cubeCenter)

cubesTriangles :: (Monad m) => Stream (Of Cube) m r -> Stream (Of (Triangle3 GLfloat)) m r
cubesTriangles cubes =
  S.for cubes (\(Cube size position) ->
    S.map (translateTriangle3 position) (
      S.map (size *^) (
        S.each cubeTriangles)))

cubesNormals :: (Monad m) => Stream (Of Cube) m r -> Stream (Of (Triangle3 GLfloat)) m r
cubesNormals voxels = S.for voxels (\_ -> S.each cubeNormals)

createChunk :: Int -> Resolution -> (Cube -> Side) -> IO Chunk
createChunk depth resolution volume = do

  let cubes = sample depth resolution volume (Cube 1 (V3 0 0 0))

  S.length_ cubes >>= print

  trianglePositions <- S.toList_ (cubesTriangles cubes)
  triangleNormals <- S.toList_ (cubesNormals cubes)

  uploadData trianglePositions triangleNormals


type ShaderProgram = GLuint
type Uniform = GLint
type VertexBufferObject = GLuint
type VertexArrayObject = GLuint


data Chunk = Chunk {
  _numberOfTriangles :: GLuint,
  _shaderProgram :: ShaderProgram,
  _cameraMatrixUniform :: Uniform,
  _vertexPositionBufferObject :: VertexBufferObject,
  _vertexNormalBufferObject :: VertexBufferObject,
  _vertexArrayObject :: VertexArrayObject }


uploadData :: [Triangle3 GLfloat] -> [Triangle3 GLfloat] -> IO Chunk
uploadData trianglePositions triangleNormals = do

  -- number of triangles
  let _numberOfTriangles = fromIntegral (length trianglePositions)

  -- fill position VBO
  _vertexPositionBufferObject <- alloca (\vboPtr -> do
    glGenBuffers 1 vboPtr
    vbo <- peek vboPtr
    glBindBuffer GL_ARRAY_BUFFER vbo
    withArrayLen trianglePositions (\len trianglePositionsPtr -> do
      let sizeOfTriangle = sizeOf (undefined :: Triangle3 GLfloat)
          primitiveDataSize = fromIntegral (len * fromIntegral sizeOfTriangle)
      glBufferData
         GL_ARRAY_BUFFER
         primitiveDataSize
         (castPtr trianglePositionsPtr)
         GL_STATIC_DRAW)
    return vbo)

  -- fill normal VBO
  _vertexNormalBufferObject <- alloca (\vboPtr -> do
    glGenBuffers 1 vboPtr
    vbo <- peek vboPtr
    glBindBuffer GL_ARRAY_BUFFER vbo
    withArrayLen triangleNormals (\len triangleNormalsPtr -> do
      let sizeOfTriangle = sizeOf (undefined :: Triangle3 GLfloat)
          primitiveDataSize = fromIntegral (len * fromIntegral sizeOfTriangle)
      glBufferData
         GL_ARRAY_BUFFER
         primitiveDataSize
         (castPtr triangleNormalsPtr)
         GL_STATIC_DRAW)
    return vbo)

  -- create shaders
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  withCString vertexShaderSource (\vertexShaderSourceCString -> do
    with vertexShaderSourceCString (\vertexShaderSourceCStringPtr -> do
      glShaderSource vertexShader 1 vertexShaderSourceCStringPtr nullPtr))
  glCompileShader vertexShader

  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  withCString fragmentShaderSource (\fragmentShaderSourceCString -> do
    with fragmentShaderSourceCString (\fragmentShaderSourceCStringPtr -> do
      glShaderSource fragmentShader 1 fragmentShaderSourceCStringPtr nullPtr))
  glCompileShader fragmentShader

  -- create program
  _shaderProgram <- glCreateProgram
  glAttachShader _shaderProgram vertexShader
  glAttachShader _shaderProgram fragmentShader
  glLinkProgram _shaderProgram

  -- find attributes
  vertexPositionAttribute <- withCString "vertex_position" (\vertexPositionCString ->
    glGetAttribLocation _shaderProgram vertexPositionCString >>= checkLocationSign)
  vertexNormalAttribute <- withCString "vertex_normal" (\vertexNormalCString ->
    glGetAttribLocation _shaderProgram vertexNormalCString >>= checkLocationSign)

  -- find camera uniform
  _cameraMatrixUniform <- withCString "camera_matrix" (\cameraMatrixCString ->
    glGetUniformLocation _shaderProgram cameraMatrixCString)

  -- delete shaders
  glDeleteShader fragmentShader
  glDeleteShader vertexShader

 -- define VAO
  _vertexArrayObject <- alloca (\vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    vao <- peek vaoPtr
    glBindVertexArray vao
    glEnableVertexAttribArray vertexPositionAttribute
    glBindBuffer GL_ARRAY_BUFFER _vertexPositionBufferObject
    glVertexAttribPointer vertexPositionAttribute 3 GL_FLOAT GL_FALSE 0 nullPtr
    glEnableVertexAttribArray vertexNormalAttribute
    glBindBuffer GL_ARRAY_BUFFER _vertexNormalBufferObject
    glVertexAttribPointer vertexNormalAttribute 3 GL_FLOAT GL_FALSE 0 nullPtr
    return vao)

  return (Chunk {..})


renderChunk :: Camera -> Chunk -> IO ()
renderChunk camera Chunk{..} = do

  --set camera
  let viewProjectionMatrix = projectionMatrix !*! cameraMatrix camera
  with viewProjectionMatrix (\cameraMatrixPtr ->
    glUniformMatrix4fv _cameraMatrixUniform 1 GL_TRUE (castPtr cameraMatrixPtr))

  -- render
  glUseProgram _shaderProgram
  glBindVertexArray _vertexArrayObject
  let n = fromIntegral _numberOfTriangles
  glDrawArrays GL_TRIANGLES 0 (3 * n)


projectionMatrix :: M44 GLfloat
projectionMatrix = perspective 1 1 0.001 1000


deleteChunk :: Chunk -> IO ()
deleteChunk Chunk{..} = do

  -- delete
  glDeleteProgram _shaderProgram
  with _vertexArrayObject (\vertexArrayObjectPtr -> do
    glDeleteVertexArrays 1 vertexArrayObjectPtr)
  with _vertexPositionBufferObject (\vertexBufferObjectPtr -> do
    glDeleteBuffers 1 vertexBufferObjectPtr)
  with _vertexNormalBufferObject (\vertexBufferObjectPtr -> do
    glDeleteBuffers 1 vertexBufferObjectPtr)


cubeTriangles :: [Triangle3 GLfloat]
cubeTriangles =
  faceAtPositionSpannedBy (V3 0 0 0) (V3 0 1 0) (V3 1 0 0) ++
  faceAtPositionSpannedBy (V3 0 0 1) (V3 1 0 0) (V3 0 1 0) ++
  faceAtPositionSpannedBy (V3 0 0 0) (V3 0 0 1) (V3 0 1 0) ++
  faceAtPositionSpannedBy (V3 1 0 0) (V3 0 1 0) (V3 0 0 1) ++
  faceAtPositionSpannedBy (V3 0 0 0) (V3 1 0 0) (V3 0 0 1) ++
  faceAtPositionSpannedBy (V3 0 1 0) (V3 0 0 1) (V3 1 0 0)

faceAtPositionSpannedBy :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> [Triangle3 GLfloat]
faceAtPositionSpannedBy p a b = [triangle1, triangle2] where
  triangle1 = Triangle3 p (p + a) (p + a + b)
  triangle2 = Triangle3 p (p + a + b) (p + b)

cubeNormals :: [Triangle3 GLfloat]
cubeNormals = [
  Triangle3 (V3 0 0 (-1)) (V3 0 0 (-1)) (V3 0 0 (-1)),
  Triangle3 (V3 0 0 (-1)) (V3 0 0 (-1)) (V3 0 0 (-1)),
  Triangle3 (V3 0 0 1) (V3 0 0 1) (V3 0 0 1),
  Triangle3 (V3 0 0 1) (V3 0 0 1) (V3 0 0 1),
  Triangle3 (V3 (-1) 0 0) (V3 (-1) 0 0) (V3 (-1) 0 0),
  Triangle3 (V3 (-1) 0 0) (V3 (-1) 0 0) (V3 (-1) 0 0),
  Triangle3 (V3 1 0 0) (V3 1 0 0) (V3 1 0 0),
  Triangle3 (V3 1 0 0) (V3 1 0 0) (V3 1 0 0),
  Triangle3 (V3 0 (-1) 0) (V3 0 (-1) 0) (V3 0 (-1) 0),
  Triangle3 (V3 0 (-1) 0) (V3 0 (-1) 0) (V3 0 (-1) 0),
  Triangle3 (V3 0 1 0) (V3 0 1 0) (V3 0 1 0),
  Triangle3 (V3 0 1 0) (V3 0 1 0) (V3 0 1 0)]

vertexShaderSource :: String
vertexShaderSource = "\
\#version 130\n\
\uniform mat4 camera_matrix;\
\uniform vec3 position_vector;\
\in vec3 vertex_position;\
\in vec3 vertex_normal;\
\out vec3 color;\
\void main () {\
\  gl_Position = camera_matrix * vec4 (position_vector + vertex_position, 1.0);\
\  color = vertex_normal;\
\}"

fragmentShaderSource :: String
fragmentShaderSource = "\
\#version 130\n\
\in vec3 color;\
\out vec4 frag_colour;\
\void main () {\
\  frag_colour = vec4 (0.5 * (vec3(1, 1, 1) + color), 1.0);\
\}"

data Triangle3 a = Triangle3 !(V3 a) !(V3 a) !(V3 a)
  deriving (Eq,Ord,Show,Read,Functor)

translateTriangle3 :: (Num a) => V3 a -> Triangle3 a -> Triangle3 a
translateTriangle3 t (Triangle3 a b c) =
  Triangle3 (a ^+^ t) (b ^+^ t) (c ^+^ t)

scaleTriangle3 :: (Num a) => V3 a -> Triangle3 a -> Triangle3 a
scaleTriangle3 s (Triangle3 a b c) =
  Triangle3 (liftA2 (*) s a) (liftA2 (*) s b) (liftA2 (*) s c)

instance (Storable a) => Storable (Triangle3 a) where
  sizeOf _ = 3 * sizeOf (undefined :: V3 a)
  alignment _ = alignment (undefined :: V3 a)
  poke ptr (Triangle3 a b c) =
    poke ptr' a >> pokeElemOff ptr' 1 b >> pokeElemOff ptr' 2 c
      where ptr' = castPtr ptr
  peek ptr =
    liftA3 Triangle3 (peek ptr') (peekElemOff ptr' 1) (peekElemOff ptr' 2)
      where ptr' = castPtr ptr


checkLocationSign :: GLint -> IO GLuint
checkLocationSign attributeLocation = if attributeLocation < 0
  then error "Attribute location not found"
  else return (fromIntegral attributeLocation)

checkError :: IO ()
checkError = do
  errorCode <- glGetError
  print errorCode


checkShaderErrors :: ShaderProgram -> IO ()
checkShaderErrors shaderProgram = withCString longString (\shaderErrorCString -> do
  with 12 (\shaderErrorLengthPtr -> do
    glGetProgramInfoLog shaderProgram 100 shaderErrorLengthPtr shaderErrorCString
    shaderError <- peekCString shaderErrorCString
    putStrLn shaderError))

longString :: String
longString = take 100 (repeat 'a')
