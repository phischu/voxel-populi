{-# language DeriveGeneric #-}
module Grid where

import Voxel (
  Path(Path), Resolution, Location,
  Block(Air, Solid),
  Cube, Side(..), pathCube,
  Face, cubeFaces, cubeFace, Sign(..), Axis(..))

import Linear (
  V3(V3), (^+^))

import Streaming (
  Stream, Of, lift)

import qualified Streaming.Prelude as S (
  for, yield, each)

import Data.Array (
  range, inRange)
import Data.Array.IO (
  IOArray, newArray, writeArray, readArray, getBounds)

import Control.DeepSeq (NFData(rnf))

import Data.Foldable (for_)
import Control.Applicative (liftA2, liftA3)


data Grid a = Grid !Resolution !(IOArray Location a)

instance (NFData a) => NFData (Grid a) where
  rnf (Grid _ _) = ()

fromVolume :: Resolution -> (Cube -> Side) -> Path -> IO (Grid Block)
fromVolume resolution volume path = do
  grid@(Grid _ values) <- emptyGrid resolution
  for_ (pathLocations resolution path) (\location ->
    case volume (pathCube (Path resolution location)) of
      Inside -> writeArray values location Solid
      _ -> return ())
  return grid

emptyGrid :: Resolution -> IO (Grid Block)
emptyGrid resolution = do
  values <- newArray (resolutionBounds resolution) Air
  return (Grid resolution values)

resolutionBounds :: Resolution -> (Location, Location)
resolutionBounds resolution = (V3 0 0 0, pure resolution - 1)

setVoxel :: Grid a -> Path -> a -> IO (Grid a)
setVoxel (Grid resolution values) path value = do
  for_ (pathLocations resolution path) (\location ->
    writeArray values location value)
  return (Grid resolution values)

enumerate :: Grid a -> Stream (Of a) IO ()
enumerate (Grid resolution values) =
  S.for (S.each (range (resolutionBounds resolution))) (\location -> do
    value <- lift (readArray values location)
    S.yield value)

pathLocations :: Resolution -> Path -> [Location]
pathLocations resolution (Path voxelResolution voxelLocation) = do
    let relativeResolution =
          resolution `div` voxelResolution
        relativeLocation =
          fmap (\x -> x * resolution `div` voxelResolution) voxelLocation
        locations = [0 .. relativeResolution - 1]
    i <- liftA3 V3 locations locations locations
    return (relativeLocation ^+^ i)


stupidMesh :: Grid Block -> Stream (Of Face) IO ()
stupidMesh (Grid resolution blocks) =
  S.for (S.each (range (resolutionBounds resolution))) (\location -> do
    block <- lift (readArray blocks location)
    case block of
      Air -> return ()
      Solid -> S.each (cubeFaces (pathCube (Path resolution location))))


naiveMesh :: Grid Block -> Stream (Of Face) IO ()
naiveMesh (Grid resolution blocks) =
  S.for (S.each (range (resolutionBounds resolution))) (\location -> do
    block <- lift (readArray blocks location)
    case block of
      Air -> return ()
      Solid -> do
        S.for (S.each directions) (\(sign, axis) -> do
          neighbour <- lift (getNeighbour blocks location sign axis)
          case neighbour of
            Solid -> return ()
            Air -> S.yield (
              cubeFace sign axis (pathCube (Path resolution location)))))

directions :: [(Sign, Axis)]
directions = liftA2 (,) [Positive, Negative] [X, Y, Z]

getNeighbour :: IOArray Location Block -> Location -> Sign -> Axis -> IO Block
getNeighbour blocks location sign axis =
  readArrayDefault blocks (location ^+^ locationDifference) where
    locationDifference = case sign of
      Positive -> case axis of
        X -> V3 1 0 0
        Y -> V3 0 1 0
        Z -> V3 0 0 1
      Negative -> case axis of
        X -> V3 (-1) 0 0
        Y -> V3 0 (-1) 0
        Z -> V3 0 0 (-1)

readArrayDefault :: IOArray Location Block -> Location -> IO Block
readArrayDefault blocks location = do
  locationBounds <- getBounds blocks
  case inRange locationBounds location of
    False -> return Air
    True -> readArray blocks location


