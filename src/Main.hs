{-# language DeriveFunctor, ScopedTypeVariables, RecordWildCards #-}
module Main where

import qualified Graphics.UI.GLFW as GLFW (
  Window,
  init, terminate,
  createWindow, makeContextCurrent, windowShouldClose,
  pollEvents, swapBuffers, getTime,
  getKey, KeyState(..), Key(..))

import Graphics.GL

import Linear (
  V3(V3), (*^), (^+^), (^-^), dot, cross, normalize, quadrance,
  M44, mkTransformation, perspective, (!*!), transpose,
  Quaternion(Quaternion), rotate)

import Foreign.Storable (
  Storable(..), peek, sizeOf)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal (alloca, with)
import Foreign.Marshal.Array (withArrayLen)
import Data.Bits ((.|.))

import Text.Printf (printf)
import Control.Monad (unless,guard)
import Control.Applicative (liftA3)

main :: IO ()
main = do

  _ <- GLFW.init

  Just window <- GLFW.createWindow 600 600 "Voxel Populi" Nothing Nothing

  GLFW.makeContextCurrent (Just window)

  Just time <- GLFW.getTime

  renderState <- initRenderState

  loop window time renderState initialCamera

  clear renderState

  GLFW.terminate


data Camera = Camera {
  _cameraPosition :: V3 GLfloat,
  _cameraOrientation :: Quaternion GLfloat }

lookAt :: V3 GLfloat -> V3 GLfloat -> Camera
lookAt eye center = Camera eye orientation where
  orientation = quaternionBetweenVectors negativeZ direction
  negativeZ = V3 0 0 (-1)
  direction = normalize (center ^-^ eye)

quaternionBetweenVectors :: V3 GLfloat -> V3 GLfloat -> Quaternion GLfloat
quaternionBetweenVectors x y =
  normalize (1 + Quaternion (dot x y) (cross x y))

initialCamera :: Camera
initialCamera = lookAt (V3 40 40 30) (V3 0 0 0)

yaw :: GLfloat -> Camera -> Camera
yaw x (Camera position orientation) = Camera position orientation

fly :: V3 GLfloat -> Camera -> Camera
fly direction (Camera position orientation) =
  Camera (position ^+^ rotate orientation direction) orientation

cameraMatrix :: Camera -> M44 GLfloat
cameraMatrix (Camera position orientation) =
  mkTransformation inverseOrientation inversePosition where
    inverseOrientation = 1 / orientation
    inversePosition = rotate inverseOrientation (negate position)

projectionMatrix :: M44 GLfloat
projectionMatrix = perspective 1 1 0.001 1000


type Resolution = Int
type Volume = V3 GLfloat -> Bool


ball :: Volume
ball x = quadrance (x ^-^ (V3 8 8 8)) < 15 * 15


renderVolume :: Camera -> RenderState -> Resolution -> Volume -> IO ()
renderVolume camera renderState resolution volume = do

  let viewProjectionMatrix = projectionMatrix !*! cameraMatrix camera

  with viewProjectionMatrix (\cameraMatrixPtr ->
    glUniformMatrix4fv (_cameraMatrixUniform renderState) 1 GL_TRUE (castPtr cameraMatrixPtr))

  sequence_ (do
    i1 <- [0 .. resolution - 1]
    i2 <- [0 .. resolution - 1]
    i3 <- [0 .. resolution - 1]
    let position = fmap fromIntegral (V3 i1 i2 i3)
    guard (volume position)
    return (renderCube renderState position))


renderCube :: RenderState -> V3 GLfloat -> IO ()
renderCube RenderState{..} positionVector = do

  -- render
  glUseProgram _shaderProgram
  with positionVector (\positionVectorPtr ->
    glUniform3fv _positionVectorUniform 1 (castPtr positionVectorPtr))
  glBindVertexArray _vertexArrayObject
  let n = fromIntegral (length cubeTriangles)
  glDrawArrays GL_TRIANGLES 0 (3 * n)



data RenderState = RenderState {
  _shaderProgram :: ShaderProgram,
  _cameraMatrixUniform :: Uniform,
  _positionVectorUniform :: Uniform,
  _vertexPositionBufferObject :: VertexBufferObject,
  _vertexNormalBufferObject :: VertexBufferObject,
  _vertexArrayObject :: VertexArrayObject }


type ShaderProgram = GLuint
type Uniform = GLint
type VertexBufferObject = GLuint
type VertexArrayObject = GLuint


initRenderState :: IO RenderState
initRenderState = do

  -- global attributes
  glEnable GL_DEPTH_TEST
  glClearColor 1 1 1 1

  -- fill position VBO
  _vertexPositionBufferObject <- alloca (\vboPtr -> do
    glGenBuffers 1 vboPtr
    vbo <- peek vboPtr
    glBindBuffer GL_ARRAY_BUFFER vbo
    withArrayLen cubeTriangles (\len trianglesPtr -> do
      let sizeOfTriangle = sizeOf (undefined :: Triangle3 GLfloat)
          primitiveDataSize = fromIntegral (len * fromIntegral sizeOfTriangle)
      glBufferData
         GL_ARRAY_BUFFER
         primitiveDataSize
         (castPtr trianglesPtr)
         GL_STATIC_DRAW)
    return vbo)

  -- fill normal VBO
  _vertexNormalBufferObject <- alloca (\vboPtr -> do
    glGenBuffers 1 vboPtr
    vbo <- peek vboPtr
    glBindBuffer GL_ARRAY_BUFFER vbo
    withArrayLen cubeNormals (\len triangleNormalsPtr -> do
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

  -- find uniforms
  _cameraMatrixUniform <- withCString "camera_matrix" (\cameraMatrixCString ->
    glGetUniformLocation _shaderProgram cameraMatrixCString)
  _positionVectorUniform <- withCString "position_vector" (\positionVectorCString ->
    glGetUniformLocation _shaderProgram positionVectorCString)

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

  return (RenderState {..})


loop :: GLFW.Window -> Double -> RenderState -> Camera -> IO ()
loop window lastTime renderState camera = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  renderVolume camera renderState 32 ball

  GLFW.swapBuffers window

  _ <- GLFW.pollEvents

  keystateW <- GLFW.getKey window GLFW.Key'W

  let movement = case keystateW of
        GLFW.KeyState'Pressed -> V3 0 0 (-0.1)
        GLFW.KeyState'Released -> V3 0 0 0

  let camera' = fly movement camera

  Just thisTime <- GLFW.getTime

  printf "Frametime: %4.4f\n" (thisTime - lastTime)

  shouldClose <- GLFW.windowShouldClose window

  unless shouldClose (loop window thisTime renderState camera')


clear :: RenderState -> IO ()
clear RenderState{..} = do

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
