{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
module Octree where

import Voxel (
  Path(Path), Location, rootPath, appendPath,
  Leaf(Leaf), Block(Air, Solid),
  Depth, Cube, Side(..), pathCube, cubeFaces,
  Face, cubeFace, Sign(Positive, Negative), Axis(X, Y, Z))

import Linear (
  V3(V3))

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  each, mapMaybe)

import Control.DeepSeq (
  NFData)

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

splitVoxel :: Path -> Maybe (Location, Path)
splitVoxel (Path 1 (V3 0 0 0)) =
  Nothing
splitVoxel (Path resolution location) =
  Just (childLocation, Path parentResolution parentLocation) where
    parentResolution = resolution `div` 2
    parentLocation = fmap (`mod` parentResolution) location
    childLocation = fmap (`div` parentResolution) location

fromVolume :: Depth -> (Cube -> Side) -> Path -> Octree Block
fromVolume depth volume path
  | depth < 0 = Full Air
  | otherwise = case volume (pathCube path) of
    Outside -> Full Air
    Inside -> Full Solid
    Border -> summarize (Children (
      mapOct (fromVolume (depth - 1) volume . appendPath path) octVoxels))

summarize :: (Eq a) => Octree a -> Octree a
summarize (Children children) =
  case nub (octToList (mapOct isFull children)) of
    [Just a] -> Full a
    _ -> Children children where
summarize octree =
  octree

isFull :: Octree a -> Maybe a
isFull (Full value) = Just value
isFull _ = Nothing

zipOctree :: Octree a -> Octree b -> Octree (a, b)
zipOctree (Full value1) (Full value2) =
  Full (value1, value2)
zipOctree (Full value1) (Children children2) =
  Children (zipOctWith zipOctree (homogeneousOct (Full value1)) children2)
zipOctree (Children children1) (Full value2) =
  Children (zipOctWith zipOctree children1 (homogeneousOct (Full value2)))
zipOctree (Children children1) (Children children2) =
  Children (zipOctWith zipOctree children1 children2)

setVoxel :: (Eq a) => Octree a -> Path -> a -> Octree a
setVoxel _ (Path 1 (V3 0 0 0)) value =
  Full value
setVoxel (Full octreeValue) path value =
  setVoxel (Children (homogeneousOct (Full octreeValue))) path value
setVoxel (Children octreeChildren) path value =
  summarize (Children (mapChild child (\octree ->
    setVoxel octree rest value) octreeChildren)) where
      Just (child, rest) = splitVoxel path

enumerate :: Octree a -> [Leaf a]
enumerate octree = enumerateRelative octree rootPath

enumerateRelative :: Octree a -> Path -> [Leaf a]
enumerateRelative (Full a) path =
  [Leaf path a]
enumerateRelative (Children children) path =
  concat (octToList (zipOctWith enumerateRelative children (childVoxels path)))

childVoxels :: Path -> Oct Path
childVoxels path =
  zipOctWith appendPath (homogeneousOct path) octVoxels

octVoxels :: Oct Path
octVoxels = Oct
  (Path 2 (V3 0 0 0))
  (Path 2 (V3 1 0 0))
  (Path 2 (V3 0 1 0))
  (Path 2 (V3 1 1 0))
  (Path 2 (V3 0 0 1))
  (Path 2 (V3 1 0 1))
  (Path 2 (V3 0 1 1))
  (Path 2 (V3 1 1 1))


toMesh :: (Monad m) => Octree Block -> Stream (Of Face) m ()
toMesh octree = do
  octreeFaces Positive X octree
  octreeFaces Positive Y octree
  octreeFaces Positive Z octree
  octreeFaces Negative X octree
  octreeFaces Negative Y octree
  octreeFaces Negative Z octree


octreeFaces :: (Monad m) => Sign -> Axis -> Octree Block -> Stream (Of Face) m ()
octreeFaces sign axis octree =
  S.mapMaybe (maybeVoxelFace sign axis) (
    S.each (enumerate octreeWithNeighbour)) where
      octreeWithNeighbour = perhapsUnTranspose (perhapsMirror (
        neighbours (perhapsMirror (perhapsTranspose octree)) (Full Air)))
      perhapsMirror = case sign of
        Negative -> mirrorOctree
        Positive -> id
      perhapsTranspose = case axis of
        X -> id
        Y -> transposeOctree
        Z -> transposeOctree . transposeOctree
      perhapsUnTranspose = case axis of
        X -> id
        Y -> transposeOctree . transposeOctree
        Z -> transposeOctree


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

maybeVoxelFace :: Sign -> Axis -> Leaf (Block, Block) -> Maybe Face
maybeVoxelFace sign axis (Leaf path (Solid, Air)) =
  Just (cubeFace sign axis (pathCube path))
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

stupidMesh :: Octree Block -> [Face]
stupidMesh octree =
  concatMap leafFaces (enumerate octree)

leafFaces :: Leaf Block -> [Face]
leafFaces (Leaf _ Air) =
  []
leafFaces (Leaf path Solid) =
  cubeFaces (pathCube path)

