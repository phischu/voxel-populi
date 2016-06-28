{-# language DeriveFunctor, ScopedTypeVariables, RecordWildCards #-}
module Main where

import Camera (
  Camera, lookAt, fly, pan)
import Voxel (
  Path(Path), Block(Air, Solid), rootPath)
import qualified Grid (
  fromVolume, stupidMesh, naiveMesh)
import Octree (
  Octree(Full), fromVolume, naiveMesh, stupidMesh,
  setVoxel)
import Volumes (
  ball, cave)
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

import qualified Streaming.Prelude as S (
  each, length_)

import Linear (
  V2(V2), V3(V3), (*^))

import Data.Bits ((.|.))

import Text.Printf (printf)
import Control.Monad (when, unless)

depth :: Int
depth = 3

resolution :: Int
resolution = 8

octree :: Octree Block
octree = ballOctree

miniOctree :: Octree Block
miniOctree = setVoxel (Full Solid) (Path 2 (V3 1 1 1)) Air

ballOctree :: Octree Block
ballOctree = fromVolume depth ball rootPath

caveOctree :: Octree Block
caveOctree = fromVolume depth cave rootPath

initialCamera :: Camera
initialCamera = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 1 0)

wireframe :: Bool
wireframe = True

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

  ballGrid <- Grid.fromVolume resolution ball rootPath
  gpuMesh <- createGPUMesh (S.each (Octree.naiveMesh octree))

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


