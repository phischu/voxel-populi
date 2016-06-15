{-# LANGUAGE DeriveGeneric #-}
module Voxel where

import Linear (
  V3(V3), (*^), (^+^))

import Control.Applicative (liftA3)


type Depth = Int
type Resolution = Int
type Location = V3 Int

data Voxel = Voxel Resolution Location
  deriving (Show, Eq, Ord)

data Cube = Cube Float (V3 Float)
  deriving (Show, Eq, Ord)

data Face = Face (V3 Float) (V3 Float) (V3 Float)
  deriving (Show, Eq, Ord)

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

volumeVoxels ::
  Depth ->
  Resolution ->
  (Cube -> Side) ->
  Voxel ->
  [Voxel]
volumeVoxels depth resolution volume voxel
  | depth < 0 = []
  | otherwise = case volume (voxelCube voxel) of
    Outside -> []
    Inside -> [voxel]
    Border -> do
      childVoxel <- childVoxels resolution voxel
      volumeVoxels (depth - 1) resolution volume childVoxel

unitVoxel :: Voxel
unitVoxel = Voxel 1 (V3 0 0 0)

relativeVoxel :: Voxel -> Voxel -> Voxel
relativeVoxel parentVoxel childVoxel =
  Voxel resolution location where
    resolution = parentResolution * childResolution
    location = childResolution *^ parentLocation ^+^ childLocation
    Voxel parentResolution parentLocation = parentVoxel
    Voxel childResolution childLocation = childVoxel

childVoxels :: Resolution -> Voxel -> [Voxel]
childVoxels resolution voxel =
  map (relativeVoxel voxel) (subdivideVoxels resolution)

subdivideVoxels :: Resolution -> [Voxel]
subdivideVoxels resolution = do
  let locations = [0 .. resolution - 1]
  i <- liftA3 V3 locations locations locations
  return (Voxel resolution i)

voxelCube :: Voxel -> Cube
voxelCube (Voxel resolution location) = Cube size position where
  size = recip (realToFrac resolution)
  position = size *^ fmap realToFrac location
