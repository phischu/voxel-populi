{-# LANGUAGE DeriveGeneric #-}
module Voxel where

import Linear (
  V3(V3), (*^), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, for, yield, each)

import Control.Applicative (liftA3)


type Depth = Int
type Resolution = Int
type Location = V3 Int
data Voxel = Voxel Resolution Location

data Cube = Cube Float (V3 Float)
  deriving (Show, Eq, Ord)

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

volumeVoxels ::
  (Monad m) =>
  Depth ->
  Resolution ->
  (Cube -> Side) ->
  Voxel ->
  Stream (Of Voxel) m ()
volumeVoxels depth resolution volume voxel
  | depth < 0 = return ()
  | otherwise = case volume (voxelCube voxel) of
    Outside -> return ()
    Inside -> S.yield voxel
    Border -> S.for (childVoxels resolution voxel) (\childVoxel ->
      volumeVoxels (depth - 1) resolution volume childVoxel)

unitVoxel :: Voxel
unitVoxel = Voxel 1 (V3 0 0 0)

relativeVoxel :: Voxel -> Voxel -> Voxel
relativeVoxel parentVoxel childVoxel =
  Voxel resolution location where
    resolution = parentResolution * childResolution
    location = childResolution *^ parentLocation ^+^ childLocation
    Voxel parentResolution parentLocation = parentVoxel
    Voxel childResolution childLocation = childVoxel

childVoxels :: (Monad m) => Resolution -> Voxel -> Stream (Of Voxel) m ()
childVoxels resolution voxel =
  S.map (relativeVoxel voxel) (S.each (subdivideVoxels resolution))

subdivideVoxels :: Resolution -> [Voxel]
subdivideVoxels resolution = do
  let locations = [0 .. resolution - 1]
  i <- liftA3 V3 locations locations locations
  return (Voxel resolution i)

voxelCube :: Voxel -> Cube
voxelCube (Voxel resolution location) = Cube size position where
  size = recip (realToFrac resolution)
  position = size *^ fmap realToFrac location

