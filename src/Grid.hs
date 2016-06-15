module Grid where

import Voxel (
  Voxel(Voxel), Resolution, Location,
  unitVoxel)

import Linear (
  V3(V3), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, filter, mapM, each, foldM_)

import Data.Array.IO (
  IOArray, newArray, writeArray, readArray)

import Data.Foldable (for_)
import Control.Applicative (liftA3)


data Grid a = Grid !Resolution !(IOArray Location a)


fromVoxels :: Resolution -> Stream (Of Voxel) IO r -> IO (Grid Bool)
fromVoxels resolution =
  S.foldM_
    (\grid address -> setVoxel grid address True)
    (emptyGrid resolution)
    return

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

getVoxels :: Grid Bool -> Voxel -> Stream (Of (Voxel,Bool)) IO ()
getVoxels (Grid resolution values) address =
  S.mapM (\i -> do
    value <- readArray values i
    return (Voxel resolution i, value)) (
      S.each (voxelLocations resolution address))

visibleVoxels :: Grid Bool -> Stream (Of Voxel) IO ()
visibleVoxels grid =
  S.map fst (
    S.filter snd (
      getVoxels grid unitVoxel))

voxelLocations :: Resolution -> Voxel -> [Location]
voxelLocations resolution (Voxel voxelResolution voxelLocation) = do
    let relativeResolution =
          resolution `div` voxelResolution
        relativeLocation =
          fmap (\x -> x * resolution `div` voxelResolution) voxelLocation
        locations = [0 .. relativeResolution - 1]
    i <- liftA3 V3 locations locations locations
    return (relativeLocation ^+^ i)

