{-# language DeriveFunctor, ScopedTypeVariables, RecordWildCards #-}
module Main where

import Camera (
  Camera, lookAt, fly, pan)
import Voxel (
  Cube(Cube), Side(..), volumeVoxels, unitVoxel)
import Grid (
  fromVoxels, visibleVoxels)
import Chunk (
  Chunk, createChunk, renderChunk, deleteChunk)

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
import Control.Monad (unless)

depth :: Int
depth = 6

resolution :: Int
resolution = 2

initialCamera :: Camera
initialCamera = lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 1 0)

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

main :: IO ()
main = do

  _ <- GLFW.init

  Just window <- GLFW.createWindow 600 600 "Voxel Populi" Nothing Nothing

  GLFW.makeContextCurrent (Just window)

  Just time <- GLFW.getTime
  cursorPos <- GLFW.getCursorPos window

  glEnable GL_DEPTH_TEST
  glClearColor 1 1 1 1

  let voxels = volumeVoxels depth resolution ball unitVoxel
  grid <- fromVoxels (resolution ^ depth) voxels
  chunk <- createChunk (visibleVoxels grid)

  loop window time cursorPos initialCamera chunk

  deleteChunk chunk

  GLFW.terminate

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


