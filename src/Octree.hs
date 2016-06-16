{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
module Octree where

import Voxel (
  Voxel(Voxel), Location, unitVoxel, relativeVoxel,
  Depth, Cube, Side(..), voxelCube,
  Face, voxelFace, voxelFaces)

import Linear (
  V2(V2), V3(V3), (^+^),
  _x, _y, ex)

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  each, map, concat, mapMaybe)

import Control.DeepSeq (
  NFData)

import Control.Lens (
  over)

import Control.Applicative (
  liftA2)

import GHC.Generics (
  Generic)


data Octree a =
  Full a |
  Children (Oct (Octree a))
    deriving (Eq, Ord, Show, Generic)

instance (NFData a) => NFData (Octree a)

newtype Oct a = Oct (V2 (V2 (V2 a)))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic)

instance (NFData a) => NFData (Oct a)

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

fromVolume :: Depth -> (Cube -> Side) -> Voxel -> Octree Bool
fromVolume depth volume voxel
  | depth <= 0 = Full False
  | otherwise = case volume (voxelCube voxel) of
    Outside -> Full False
    Inside -> Full True
    Border -> Children (
      fmap (fromVolume (depth - 1) volume . relativeVoxel voxel) octVoxels)

emptyOctree :: Octree Bool
emptyOctree = Full False

zipOctree :: Octree a -> Octree b -> Octree (a, b)
zipOctree (Full value1) (Full value2) =
  Full (value1, value2)
zipOctree (Full value1) (Children children2) =
  Children (zipOctWith zipOctree (homogeneousOct (Full value1)) children2)
zipOctree (Children children1) (Full value2) =
  Children (zipOctWith zipOctree children1 (homogeneousOct (Full value2)))
zipOctree (Children children1) (Children children2) =
  Children (zipOctWith zipOctree children1 children2)

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
octreeMesh octree =
  S.mapMaybe rightOctreeFace (
    S.each (
        getVoxels (octreeNeighbour octree (Full False)) unitVoxel))

octreeNeighbour :: Octree a -> Octree a -> Octree (a, a)
octreeNeighbour (Full value1) (Full value2) =
  Full (value1, value2)
octreeNeighbour (Full value1) (Children children2) =
  octreeNeighbour (Children (homogeneousOct (Full value1))) (Children children2)
octreeNeighbour (Children children1) (Full value2) =
  octreeNeighbour (Children children1) (Children (homogeneousOct (Full value2)))
octreeNeighbour (Children children1) (Children children2) =
  Children (zipOctWith octreeNeighbour children1 neighbours) where
    neighbours = Oct (V2 rightChildrenA leftChildrenB)
    (Oct (V2 _ rightChildrenA)) = children1
    (Oct (V2 leftChildrenB _)) = children2

rightOctreeFace :: (Voxel, (Bool, Bool)) -> Maybe Face
rightOctreeFace (voxel, (True, False)) =
  Just (voxelFace ex ex voxel)
rightOctreeFace _ =
  Nothing

naiveOctreeMesh :: (Monad m) => Octree Bool -> Stream (Of Face) m ()
naiveOctreeMesh octree =
  S.concat (S.concat (S.map voxelFaces (S.each (visibleVoxels octree))))

visibleVoxels :: Octree Bool -> [Voxel]
visibleVoxels octree =
  map fst (filter snd (getVoxels octree unitVoxel))

