{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
module Octree where

import Voxel (
  Voxel(Voxel), Location, unitVoxel, relativeVoxel,
  Depth, Cube, Side(..), voxelCube,
  Face, voxelFace, voxelFaces)

import Linear (
  V2(V2), V3(V3),
  E(el), ex, ey, ez)

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  each, map, concat, mapMaybe)

import Control.DeepSeq (
  NFData)

import Control.Lens (
  view)

import Data.List (
  nub)

import GHC.Generics (
  Generic)


data Octree a =
  Full a |
  Children (Oct (Octree a))
    deriving (Eq, Ord, Show, Generic)

instance (NFData a) => NFData (Octree a)

instance Functor Octree where
  fmap f (Full a) = Full (f a)
  fmap f (Children children) = Children (mapOct (fmap f) children)

data Oct a = Oct a a a a a a a a
  deriving (Eq, Ord, Show, Generic)

instance (NFData a) => NFData (Oct a)

homogeneousOct :: a -> Oct a
homogeneousOct a = Oct a a a a a a a a

mapOct :: (a -> b) -> Oct a -> Oct b
mapOct f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7) (f a8)

zipOctWith :: (a -> b -> c) -> Oct a -> Oct b -> Oct c
zipOctWith f a b =
  Oct (f a1 b1) (f a2 b2) (f a3 b3) (f a4 b4)
      (f a5 b5) (f a6 b6) (f a7 b7) (f a8 b8) where
        Oct a1 a2 a3 a4 a5 a6 a7 a8 = a
        Oct b1 b2 b3 b4 b5 b6 b7 b8 = b

octToList :: Oct a -> [a]
octToList (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  [a1, a2, a3, a4, a5, a6, a7, a8]

mapChild :: Location -> (a -> a) -> Oct a -> Oct a
mapChild (V3 0 0 0) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct (f a1) a2 a3 a4 a5 a6 a7 a8
mapChild (V3 1 0 0) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 (f a2) a3 a4 a5 a6 a7 a8
mapChild (V3 0 1 0) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 (f a3) a4 a5 a6 a7 a8
mapChild (V3 1 1 0) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 a3 (f a4) a5 a6 a7 a8
mapChild (V3 0 0 1) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 a3 a4 (f a5) a6 a7 a8
mapChild (V3 1 0 1) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 a3 a4 a5 (f a6) a7 a8
mapChild (V3 0 1 1) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 a3 a4 a5 a6 (f a7) a8
mapChild (V3 1 1 1) f (Oct a1 a2 a3 a4 a5 a6 a7 a8) =
  Oct a1 a2 a3 a4 a5 a6 a7 (f a8)
mapChild _ _ _ =
  error "Invalid location."

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
  | depth < 0 = Full False
  | otherwise = case volume (voxelCube voxel) of
    Outside -> Full False
    Inside -> Full True
    Border -> summarize (Children (
      mapOct (fromVolume (depth - 1) volume . relativeVoxel voxel) octVoxels))

summarize :: Octree Bool -> Octree Bool
summarize (Children children) =
  case nub (octToList (mapOct isFull children)) of
    [Just a] -> Full a
    _ -> Children children where
summarize octree =
  octree

isFull :: Octree Bool -> Maybe Bool
isFull (Full value) = Just value
isFull _ = Nothing

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
  summarize (Children (mapChild child (\octree ->
    setVoxel octree rest value) octreeChildren)) where
      Just (child, rest) = splitVoxel voxel

enumerate :: Octree a -> [(Voxel, a)]
enumerate octree = enumerateRelative octree unitVoxel

enumerateRelative :: Octree a -> Voxel -> [(Voxel, a)]
enumerateRelative (Full a) voxel =
  [(voxel, a)]
enumerateRelative (Children children) voxel =
  concat (octToList (zipOctWith enumerateRelative children (childVoxels voxel)))

childVoxels :: Voxel -> Oct Voxel
childVoxels voxel =
  zipOctWith relativeVoxel (homogeneousOct voxel) octVoxels

octVoxels :: Oct Voxel
octVoxels = Oct
  (Voxel 2 (V3 0 0 0))
  (Voxel 2 (V3 1 0 0))
  (Voxel 2 (V3 0 1 0))
  (Voxel 2 (V3 1 1 0))
  (Voxel 2 (V3 0 0 1))
  (Voxel 2 (V3 1 0 1))
  (Voxel 2 (V3 0 1 1))
  (Voxel 2 (V3 1 1 1))


toMesh :: (Monad m) => Octree Bool -> Stream (Of Face) m ()
toMesh octree = do
  octreeFaces ex ex octree
  octreeFaces ex ey octree
  octreeFaces ex ez octree
  octreeFaces ey ex octree
  octreeFaces ey ey octree
  octreeFaces ey ez octree

octreeFaces :: (Monad m) => E V2 -> E V3 -> Octree Bool -> Stream (Of Face) m ()
octreeFaces orientation direction octree =
  S.mapMaybe (maybeVoxelFace orientation direction) (
    S.each (enumerate octreeWithNeighbour)) where
      octreeWithNeighbour = perhapsUnTranspose (perhapsMirror (
        neighbours (perhapsMirror (perhapsTranspose octree)) (Full False)))
      perhapsMirror =
        view (el orientation) (V2 id mirrorOctree)
      perhapsTranspose =
        view (el direction)
          (V3 id transposeOctree (transposeOctree . transposeOctree))
      perhapsUnTranspose =
        view (el direction)
          (V3 id (transposeOctree . transposeOctree) transposeOctree)

mirrorOctree :: Octree a -> Octree a
mirrorOctree (Children (Oct a1 a2 a3 a4 a5 a6 a7 a8)) =
  Children (mapOct mirrorOctree (Oct a2 a1 a4 a3 a6 a5 a8 a7))
mirrorOctree octree =
  octree

transposeOctree :: Octree a -> Octree a
transposeOctree (Children (Oct a1 a2 a3 a4 a5 a6 a7 a8)) =
  Children (mapOct transposeOctree (Oct a1 a3 a5 a7 a2 a4 a6 a8))
transposeOctree octree =
  octree

maybeVoxelFace :: E V2 -> E V3 -> (Voxel, (Bool, Bool)) -> Maybe Face
maybeVoxelFace orientation direction (voxel, (True, False)) =
  Just (voxelFace orientation direction voxel)
maybeVoxelFace _ _ _ =
  Nothing

neighbours :: Octree a -> Octree a -> Octree (a, a)
neighbours (Full value1) (Full value2) =
  Full (value1, value2)
neighbours (Full value1) (Children children2) =
  neighbours (Children (homogeneousOct (Full value1))) (Children children2)
neighbours (Children children1) (Full value2) =
  neighbours (Children children1) (Children (homogeneousOct (Full value2)))
neighbours (Children children) (Children neighbourChildren) =
  Children (zipOctWith neighbours children newNeighbours) where
    (Oct _ c2 _ c4 _ c6 _ c8) = children
    (Oct n1 _ n3 _ n5 _ n7 _) = neighbourChildren
    newNeighbours = Oct c2 n1 c4 n3 c6 n5 c8 n7

toMeshStupid :: (Monad m) => Octree Bool -> Stream (Of Face) m ()
toMeshStupid octree =
  S.concat (S.concat (S.map voxelFaces (S.each (visibleVoxels octree))))

visibleVoxels :: Octree Bool -> [Voxel]
visibleVoxels octree =
  map fst (filter snd (enumerate octree))

