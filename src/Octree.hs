{-# LANGUAGE DeriveFoldable #-}
module Octree where

import Voxel (
  Voxel(Voxel), Location, unitVoxel, relativeVoxel,
  Face(Face))

import Linear (
  V2(V2), V3(V3), (^+^), (*^),
  _x, _y, _z, unit)

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  each)


import Control.Lens (
  over)

import Control.Applicative (
  liftA2)

data Octree a =
  Full a |
  Children (Oct (Octree a))
    deriving (Eq, Ord, Show)

newtype Oct a = Oct (V2 (V2 (V2 a)))
  deriving (Eq, Ord, Show, Foldable)

homogeneousOct :: a -> Oct a
homogeneousOct a = Oct (pure (pure (pure a)))

zipOctWith :: (a -> b -> c) -> Oct a -> Oct b -> Oct c
zipOctWith f (Oct oct1) (Oct oct2) =
  Oct (liftA2 (liftA2 (liftA2 f)) oct1 oct2)

mapChild :: Location -> (a -> a) -> Oct a -> Oct a
mapChild (V3 i1 i2 i3) f (Oct oct) = Oct (over child f oct) where
  child = pick i1 . pick i2 . pick i3
  pick i = case i of
    0 -> _x
    1 -> _y
    _ -> error "Invalid location."

splitVoxel :: Voxel -> Maybe (Location, Voxel)
splitVoxel (Voxel 1 (V3 0 0 0)) =
  Nothing
splitVoxel (Voxel resolution location) =
  Just (childLocation, Voxel parentResolution parentLocation) where
    parentResolution = resolution `div` 2
    parentLocation = fmap (`mod` parentResolution) location
    childLocation = fmap (`div` parentResolution) location

fromVoxels :: [Voxel] -> Octree Bool
fromVoxels [] = emptyOctree
fromVoxels (voxel : voxels) = setVoxel (fromVoxels voxels) voxel True

emptyOctree :: Octree Bool
emptyOctree = Full False

zipOctree :: Octree a -> Octree b -> Octree (a, b)
zipOctree (Full valueA) (Full valueB) =
  Full (valueA, valueB)
zipOctree (Full valueA) (Children childrenB) =
  Children (zipOctWith zipOctree (homogeneousOct (Full valueA)) childrenB)
zipOctree (Children childrenA) (Full valueB) =
  Children (zipOctWith zipOctree childrenA (homogeneousOct (Full valueB)))
zipOctree (Children childrenA) (Children childrenB) =
  Children (zipOctWith zipOctree childrenA childrenB)

setVoxel :: Octree Bool -> Voxel -> Bool -> Octree Bool
setVoxel _ (Voxel 1 (V3 0 0 0)) value =
  Full value
setVoxel (Full octreeValue) voxel value =
  setVoxel (Children (homogeneousOct (Full octreeValue))) voxel value
setVoxel (Children octreeChildren) voxel value =
  Children (mapChild child (\octree ->
    setVoxel octree rest value) octreeChildren) where
      Just (child, rest) = splitVoxel voxel

getVoxels :: Octree a -> Voxel -> [(Voxel, a)]
getVoxels (Full a) voxel =
  [(voxel, a)]
getVoxels (Children children) voxel =
  concat (zipOctWith getVoxels children (childVoxels voxel))

childVoxels :: Voxel -> Oct Voxel
childVoxels voxel =
  zipOctWith relativeVoxel (homogeneousOct voxel) octVoxels

octVoxels :: Oct Voxel
octVoxels = Oct (do
  x <- V2 (V3 0 0 0) (V3 1 0 0)
  return (do
    y <- V2 (V3 0 0 0) (V3 0 1 0)
    return (do
      z <- V2 (V3 0 0 0) (V3 0 0 1)
      return (Voxel 2 (x ^+^ y ^+^ z)))))

octreeMesh :: (Monad m) => Octree Bool -> Stream (Of Face) m ()
octreeMesh = naiveOctreeMesh

naiveOctreeMesh :: (Monad m) => Octree Bool -> Stream (Of Face) m ()
naiveOctreeMesh octree =
  S.each (concatMap voxelFaces (visibleVoxels octree))

visibleVoxels :: Octree Bool -> [Voxel]
visibleVoxels octree =
  map fst (filter snd (getVoxels octree unitVoxel))

voxelFaces :: Voxel -> [Face]
voxelFaces (Voxel resolution location) = [
  Face position1 side1 side2,
  Face position1 side2 side3,
  Face position1 side3 side1,
  Face position2 (negate side1) (negate side2),
  Face position2 (negate side2) (negate side3),
  Face position2 (negate side3) (negate side1)] where
    size = recip (realToFrac resolution)
    position1 = size *^ (fmap realToFrac location)
    position2 = position1 ^+^ V3 size size size
    side1 = size *^ (unit _x)
    side2 = size *^ (unit _y)
    side3 = size *^ (unit _z)

