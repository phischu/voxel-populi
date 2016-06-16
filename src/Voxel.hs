{-# LANGUAGE DeriveGeneric #-}
module Voxel where

import Linear (
  V2(V2), V3(V3), (*^), (^+^),
  unit, E(el), _x, _y, _z)

import Control.Lens (
  view)

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

voxelFaces :: Voxel -> V2 (V3 Face)
voxelFaces (Voxel resolution location) =
  V2
    (V3
      (Face position2 (negate side2) (negate side3))
      (Face position2 (negate side3) (negate side1))
      (Face position2 (negate side1) (negate side2)))
    (V3
      (Face position1 side2 side3)
      (Face position1 side3 side1)
      (Face position1 side1 side2)) where
        size = recip (realToFrac resolution)
        position1 = size *^ (fmap realToFrac location)
        position2 = position1 ^+^ V3 size size size
        side1 = size *^ (unit _x)
        side2 = size *^ (unit _y)
        side3 = size *^ (unit _z)

voxelFace :: E V2 -> E V3 -> Voxel -> Face
voxelFace orientation direction voxel =
  view (el orientation . el direction) (voxelFaces voxel)

