{-# language DeriveFunctor, ScopedTypeVariables, RecordWildCards #-}
module Main where

import Camera (
  Camera, lookAt, fly, pan)
import Voxel (
  Voxel(Voxel), Cube(Cube), Side(..), unitVoxel)
import Octree (
  Octree, fromVolume, toMesh,
  emptyOctree, setVoxel)
import Mesh (
  GPUMesh, createGPUMesh, renderGPUMesh, deleteGPUMesh)

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
  norm)

import Data.Bits ((.|.))

import Text.Printf (printf)
import Control.Applicative (liftA3)
import Control.Monad (when, unless)

depth :: Int
depth = 6

resolution :: Int
resolution = 2

octree :: Octree Bool
octree = fmap not caveOctree

miniOctree :: Octree Bool
miniOctree = setVoxel emptyOctree (Voxel 4 (V3 1 1 1)) True

ballOctree :: Octree Bool
ballOctree = fromVolume depth ball unitVoxel

caveOctree :: Octree Bool
caveOctree = fromVolume depth cave unitVoxel

initialCamera :: Camera
initialCamera = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 1 0)

wireframe :: Bool
wireframe = False

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

cave :: Cube -> Side
cave (Cube size position)
  | size < 0.1 && all (<0) corners = Inside
  | size > 0.1 && all (>0) corners = Outside
  | otherwise = Border where
    corners = do
      d <- liftA3 V3 [0, size] [0, size] [0, size]
      return (value (position ^+^ d))
    value x = sum (fmap (\xi -> sin (omega * xi / (2 * pi))) x)
    omega = 70

main :: IO ()
main = do

  _ <- GLFW.init

  Just window <- GLFW.createWindow 600 600 "Voxel Populi" Nothing Nothing

  GLFW.makeContextCurrent (Just window)

  Just time <- GLFW.getTime
  cursorPos <- GLFW.getCursorPos window

  glEnable GL_DEPTH_TEST
  glClearColor 1 1 1 1
  when wireframe (glPolygonMode GL_FRONT_AND_BACK GL_LINE)

  gpuMesh <- createGPUMesh (toMesh octree)

  loop window time cursorPos initialCamera gpuMesh

  deleteGPUMesh gpuMesh

  GLFW.terminate

loop :: GLFW.Window -> Double -> (Double, Double) -> Camera -> GPUMesh -> IO ()
loop window lastTime (lastCursorX, lastCursorY) camera gpuMesh = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  renderGPUMesh camera gpuMesh

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
      cameraRotationSpeed = 0.005
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
    loop window currentTime (currentCursorX, currentCursorY) camera' gpuMesh)


