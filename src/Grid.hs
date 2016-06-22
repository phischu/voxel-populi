{-# language DeriveGeneric #-}
module Grid where

import Voxel (
  Voxel(Voxel), Resolution, Location, unitVoxel,
  Cube, Side(..), voxelCube,
  Face, voxelFaces)

import Linear (
  V3(V3), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, concat, filter, for, mapM, each)

import Data.Array.IO (
  IOArray, newArray, writeArray, readArray)

import Control.DeepSeq (NFData(rnf))

import Data.Foldable (for_)
import Control.Applicative (liftA3)


data Grid a = Grid !Resolution !(IOArray Location a)

instance (NFData a) => NFData (Grid a) where
  rnf (Grid _ _) = ()

fromVolume :: Resolution -> (Cube -> Side) -> Voxel -> IO (Grid Bool)
fromVolume resolution volume voxel = do
  grid@(Grid _ values) <- emptyGrid resolution
  for_ (voxelLocations resolution voxel) (\location ->
    case volume (voxelCube (Voxel resolution location)) of
      Inside -> writeArray values location True
      _ -> return ())
  return grid

emptyGrid :: Resolution -> IO (Grid Bool)
emptyGrid resolution = do
  let locationBounds = (V3 0 0 0, pure resolution - 1)
  values <- newArray locationBounds False
  return (Grid resolution values)

setVoxel :: Grid Bool -> Voxel -> Bool -> IO (Grid Bool)
setVoxel (Grid resolution values) address value = do
  for_ (voxelLocations resolution address) (\location ->
    writeArray values location value)
  return (Grid resolution values)

enumerate :: Grid Bool -> Stream (Of (Voxel,Bool)) IO ()
enumerate (Grid resolution values) =
  S.mapM (\i -> do
    value <- readArray values i
    return (Voxel resolution i, value)) (
      S.each (voxelLocations resolution unitVoxel))

voxelLocations :: Resolution -> Voxel -> [Location]
voxelLocations resolution (Voxel voxelResolution voxelLocation) = do
    let relativeResolution =
          resolution `div` voxelResolution
        relativeLocation =
          fmap (\x -> x * resolution `div` voxelResolution) voxelLocation
        locations = [0 .. relativeResolution - 1]
    i <- liftA3 V3 locations locations locations
    return (relativeLocation ^+^ i)

toMesh :: Grid Bool -> Stream (Of Face) IO ()
toMesh = toMeshStupid

toMeshStupid :: Grid Bool -> Stream (Of Face) IO ()
toMeshStupid grid = S.for (visibleVoxels grid) (\voxel ->
  S.concat (S.each (voxelFaces voxel)))

visibleVoxels :: Grid Bool -> Stream (Of Voxel) IO ()
visibleVoxels grid =
  S.map fst (
    S.filter snd (
      enumerate grid))


