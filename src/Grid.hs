{-# language DeriveGeneric #-}
module Grid where

import Voxel (
  Path(Path), Resolution, Location, rootPath,
  Cube, Side(..), pathCube,
  Face, cubeFaces)

import Linear (
  V3(V3), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, filter, for, mapM, each)

import Data.Array.IO (
  IOArray, newArray, writeArray, readArray)

import Control.DeepSeq (NFData(rnf))

import Data.Foldable (for_)
import Control.Applicative (liftA3)


data Grid a = Grid !Resolution !(IOArray Location a)

instance (NFData a) => NFData (Grid a) where
  rnf (Grid _ _) = ()

fromVolume :: Resolution -> (Cube -> Side) -> Path -> IO (Grid Bool)
fromVolume resolution volume voxel = do
  grid@(Grid _ values) <- emptyGrid resolution
  for_ (voxelLocations resolution voxel) (\location ->
    case volume (pathCube (Path resolution location)) of
      Inside -> writeArray values location True
      _ -> return ())
  return grid

emptyGrid :: Resolution -> IO (Grid Bool)
emptyGrid resolution = do
  let locationBounds = (V3 0 0 0, pure resolution - 1)
  values <- newArray locationBounds False
  return (Grid resolution values)

setVoxel :: Grid Bool -> Path -> Bool -> IO (Grid Bool)
setVoxel (Grid resolution values) address value = do
  for_ (voxelLocations resolution address) (\location ->
    writeArray values location value)
  return (Grid resolution values)

enumerate :: Grid Bool -> Stream (Of (Path,Bool)) IO ()
enumerate (Grid resolution values) =
  S.mapM (\i -> do
    value <- readArray values i
    return (Path resolution i, value)) (
      S.each (voxelLocations resolution rootPath))

voxelLocations :: Resolution -> Path -> [Location]
voxelLocations resolution (Path voxelResolution voxelLocation) = do
    let relativeResolution =
          resolution `div` voxelResolution
        relativeLocation =
          fmap (\x -> x * resolution `div` voxelResolution) voxelLocation
        locations = [0 .. relativeResolution - 1]
    i <- liftA3 V3 locations locations locations
    return (relativeLocation ^+^ i)

stupidMesh :: Grid Bool -> Stream (Of Face) IO ()
stupidMesh grid = S.for (visibleVoxels grid) (\path ->
  S.each (cubeFaces (pathCube path)))

visibleVoxels :: Grid Bool -> Stream (Of Path) IO ()
visibleVoxels grid =
  S.map fst (
    S.filter snd (
      enumerate grid))


