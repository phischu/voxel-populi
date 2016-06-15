{-# LANGUAGE DeriveFunctor, RecordWildCards, ScopedTypeVariables #-}
module Mesh where

import Voxel (Face(Face))
import Camera (Camera, cameraMatrix)

import Graphics.GL

import Linear (
  V3, (^+^), (*^),
  M44, perspective, (!*!),
  cross, norm)

import Streaming.Prelude (
  Stream, Of)
import qualified Streaming.Prelude as S (
  for, each, toList_, length_)

import Foreign.Storable (
  Storable(..), peek, sizeOf)
import Foreign.Ptr (
  castPtr, nullPtr)
import Foreign.C.String (
  withCString, peekCString)
import Foreign.Marshal (
  alloca, with)
import Foreign.Marshal.Array (
  withArrayLen)

import Control.Applicative (
  liftA2, liftA3)


type ShaderProgram = GLuint
type Uniform = GLint
type VertexBufferObject = GLuint
type VertexArrayObject = GLuint


data GPUMesh = GPUMesh {
  _numberOfTriangles :: GLuint,
  _shaderProgram :: ShaderProgram,
  _cameraMatrixUniform :: Uniform,
  _vertexPositionBufferObject :: VertexBufferObject,
  _vertexNormalBufferObject :: VertexBufferObject,
  _vertexArrayObject :: VertexArrayObject }


createGPUMesh :: Stream (Of Face) IO () -> IO GPUMesh
createGPUMesh faces = do

  S.length_ faces >>= print

  trianglePositions <- S.toList_ (facesTriangles faces)
  triangleNormals <- S.toList_ (facesNormals faces)

  uploadData trianglePositions triangleNormals


facesTriangles :: (Monad m) => Stream (Of Face) m r -> Stream (Of (Triangle3 GLfloat)) m r
facesTriangles faces = S.for faces (\(Face position side1 side2) ->
  S.each (faceAtPositionSpannedBy position side1 side2))


facesNormals :: (Monad m) => Stream (Of Face) m r -> Stream (Of (Triangle3 GLfloat)) m r
facesNormals faces = S.for faces (\(Face _ side1 side2) ->
  S.each (faceTriangleNormals side1 side2))


faceAtPositionSpannedBy :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> [Triangle3 GLfloat]
faceAtPositionSpannedBy p a b = [triangle1, triangle2] where
  triangle1 = Triangle3 p (p + a) (p + a + b)
  triangle2 = Triangle3 p (p + a + b) (p + b)


faceTriangleNormals :: V3 GLfloat -> V3 GLfloat -> [Triangle3 GLfloat]
faceTriangleNormals side1 side2 = [triangleNormals, triangleNormals] where
  triangleNormals = Triangle3 normal normal normal
  unnormalizedNormal = side1 `cross` side2
  normal = recip (norm unnormalizedNormal) *^ unnormalizedNormal


uploadData :: [Triangle3 GLfloat] -> [Triangle3 GLfloat] -> IO GPUMesh
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

  return (GPUMesh {..})


renderGPUMesh :: Camera -> GPUMesh -> IO ()
renderGPUMesh camera GPUMesh{..} = do

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


deleteGPUMesh :: GPUMesh -> IO ()
deleteGPUMesh GPUMesh{..} = do

  -- delete
  glDeleteProgram _shaderProgram
  with _vertexArrayObject (\vertexArrayObjectPtr -> do
    glDeleteVertexArrays 1 vertexArrayObjectPtr)
  with _vertexPositionBufferObject (\vertexBufferObjectPtr -> do
    glDeleteBuffers 1 vertexBufferObjectPtr)
  with _vertexNormalBufferObject (\vertexBufferObjectPtr -> do
    glDeleteBuffers 1 vertexBufferObjectPtr)

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

