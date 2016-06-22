{-# LANGUAGE DeriveGeneric #-}
module Voxel where

import Linear (
  V3(V3), (*^), (^+^))

import Control.Applicative (
  liftA3)

import Control.DeepSeq (
  NFData)

import GHC.Generics (
  Generic)


type Depth = Int

data Path = Path Resolution Location
  deriving (Show, Eq, Ord, Generic)
instance NFData Path

type Location = V3 Int

type Resolution = Int

data Cube = Cube Float (V3 Float)
  deriving (Show, Eq, Ord)

data Leaf a = Leaf Path a
  deriving (Show, Eq, Ord, Generic)

instance (NFData a) => NFData (Leaf a)

data Block = Air | Solid
  deriving (Show, Eq, Ord, Generic)

instance NFData Block

data Face = Face (V3 Float) (V3 Float) (V3 Float)
  deriving (Show, Eq, Ord, Generic)

instance NFData Face

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

pathCube :: Path -> Cube
pathCube (Path resolution location) =
  Cube size position where
    size = recip (realToFrac resolution)
    position = size *^ fmap realToFrac location

unitPath :: Path
unitPath = Path 1 (V3 0 0 0)

unitCube :: Cube
unitCube = Cube 1 (V3 0 0 0)

relativePath :: Path -> Path -> Path
relativePath parentPath childPath =
  Path resolution location where
    resolution = parentResolution * childResolution
    location = childResolution *^ parentLocation ^+^ childLocation
    Path parentResolution parentLocation = parentPath
    Path childResolution childLocation = childPath

childPaths :: Resolution -> Path -> [Path]
childPaths resolution path =
  map (relativePath path) (subdividePath resolution)

subdividePath :: Resolution -> [Path]
subdividePath resolution = do
  let locations = [0 .. resolution - 1]
  i <- liftA3 V3 locations locations locations
  return (Path resolution i)

cubeFaces :: Cube -> [Face]
cubeFaces cube = [
  cubeFace Positive X cube,
  cubeFace Positive Y cube,
  cubeFace Positive Z cube,
  cubeFace Negative X cube,
  cubeFace Negative Y cube,
  cubeFace Negative Z cube]

data Sign = Negative | Positive
  deriving (Eq, Ord, Show)

data Axis = X | Y | Z
  deriving (Eq, Ord, Show)

cubeFace :: Sign -> Axis -> Cube -> Face
cubeFace sign axis (Cube size position) =
  Face facePosition faceSide1 faceSide2 where
    facePosition = case sign of
      Negative -> position
      Positive -> position ^+^ V3 size size size
    faceSide1 = perhapsNegate (case axis of
      X -> V3 0 size 0
      Y -> V3 0 0 size
      Z -> V3 size 0 0)
    faceSide2 = perhapsNegate (case axis of
      X -> V3 0 0 size
      Y -> V3 size 0 0
      Z -> V3 0 size 0)
    perhapsNegate = case sign of
      Negative -> negate
      Positive -> id

