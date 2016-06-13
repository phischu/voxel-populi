{-# LANGUAGE DeriveFunctor, RecordWildCards, ScopedTypeVariables #-}
module Chunk where

import Voxel (addressCube, Address, Cube(Cube))
import Camera (Camera, cameraMatrix)

import Graphics.GL

import Linear (
  V3(V3), (*^), (^+^),
  M44, perspective, (!*!))

import Streaming.Prelude (
  Stream, Of)
import qualified Streaming.Prelude as S (
  map, for, each, toList_, length_)

import Foreign.Storable (
  Storable(..), peek, sizeOf)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal (alloca, with)
import Foreign.Marshal.Array (withArrayLen)

import Control.Applicative (liftA2, liftA3)


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


createChunk :: Stream (Of Address) IO () -> IO Chunk
createChunk addresses = do

  let cubes = S.map addressCube addresses

  S.length_ cubes >>= print

  trianglePositions <- S.toList_ (cubesTriangles cubes)
  triangleNormals <- S.toList_ (cubesNormals cubes)

  uploadData trianglePositions triangleNormals


cubesTriangles :: (Monad m) => Stream (Of Cube) m r -> Stream (Of (Triangle3 GLfloat)) m r
cubesTriangles cubes =
  S.for cubes (\(Cube size position) ->
    S.map (translateTriangle3 position) (
      S.map (size *^) (
        S.each cubeTriangles)))

cubesNormals :: (Monad m) => Stream (Of Cube) m r -> Stream (Of (Triangle3 GLfloat)) m r
cubesNormals voxels = S.for voxels (\_ -> S.each cubeNormals)


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
