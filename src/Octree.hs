{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
module Octree where

import Voxel (
  Path(Path), Location, rootPath, appendPath,
  Block(Air, Solid),
  Depth, Cube, Side(..), pathCube, cubeFaces,
  Face, cubeFace, Sign(Positive, Negative), Axis(X, Y, Z))

import Linear (
  V3(V3))

import Control.DeepSeq (
  NFData)

import Data.List (
  nub)
import Data.Maybe (
  mapMaybe)

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
mapOct f (Oct a0 a1 a2 a3 a4 a5 a6 a7) =
  Oct (f a0) (f a1) (f a2) (f a3) (f a4) (f a5) (f a6) (f a7)

zipOctWith :: (a -> b -> c) -> Oct a -> Oct b -> Oct c
zipOctWith f a b =
  Oct (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3)
      (f a4 b4) (f a5 b5) (f a6 b6) (f a7 b7) where
        Oct a0 a1 a2 a3 a4 a5 a6 a7 = a
        Oct b0 b1 b2 b3 b4 b5 b6 b7 = b

octToList :: Oct a -> [a]
octToList (Oct a0 a1 a2 a3 a4 a5 a6 a7) =
  [a0, a1, a2, a3, a4, a5, a6, a7]

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
      mapOct (fromVolume (depth - 1) volume . appendPath path) octPaths))

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

enumerate :: Octree a -> [a]
enumerate (Full a) =
  [a]
enumerate (Children children) =
  concatMap enumerate (octToList children)

annotatePath :: Path -> Octree a -> Octree (Path, a)
annotatePath path (Full a) =
  Full (path, a)
annotatePath path (Children children) =
  Children (zipOctWith annotatePath (childPaths path) children)

childPaths :: Path -> Oct Path
childPaths path =
  mapOct (appendPath path) octPaths

octPaths :: Oct Path
octPaths = Oct
  (Path 2 (V3 0 0 0))
  (Path 2 (V3 1 0 0))
  (Path 2 (V3 0 1 0))
  (Path 2 (V3 1 1 0))
  (Path 2 (V3 0 0 1))
  (Path 2 (V3 1 0 1))
  (Path 2 (V3 0 1 1))
  (Path 2 (V3 1 1 1))


naiveMesh :: Octree Block -> [Face]
naiveMesh octree =
  (partialNaiveMesh Positive X octree) ++
  (partialNaiveMesh Positive Y octree) ++
  (partialNaiveMesh Positive Z octree) ++
  (partialNaiveMesh Negative X octree) ++
  (partialNaiveMesh Negative Y octree) ++
  (partialNaiveMesh Negative Z octree)


partialNaiveMesh :: Sign -> Axis -> Octree Block -> [Face]
partialNaiveMesh sign axis octree =
  mapMaybe (neighbourFace sign axis) (
    (enumerate (annotatePath rootPath octreeWithNeighbour))) where
      octreeWithNeighbour = perhapsUnTranspose (perhapsMirror (
        neighbour (perhapsMirror (perhapsTranspose octree)) (Full Air)))
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

neighbourFace :: Sign -> Axis -> (Path, (Block, Block)) -> Maybe Face
neighbourFace sign axis (path, (Solid, Air)) =
  Just (cubeFace sign axis (pathCube path))
neighbourFace _ _ _ =
  Nothing

neighbour :: Octree a -> Octree a -> Octree (a, a)
neighbour (Full value) (Full neighbourValue) =
  Full (value, neighbourValue)
neighbour (Full value) (Children neighbourChildren) =
  neighbour
    (Children (homogeneousOct (Full value)))
    (Children neighbourChildren)
neighbour (Children children) (Full neighbourValue) =
  neighbour
    (Children children)
    (Children (homogeneousOct (Full neighbourValue)))
neighbour (Children children) (Children neighbourChildren) =
  Children (zipOctWith neighbour children newNeighbours) where
    (Oct _ c1 _ c3 _ c5 _ c7) = children
    (Oct n0 _ n2 _ n4 _ n6 _) = neighbourChildren
    newNeighbours = Oct c1 n0 c3 n2 c5 n4 c7 n6

stupidMesh :: Octree Block -> [Face]
stupidMesh octree =
  concatMap leafFaces (enumerate (annotatePath rootPath octree))

leafFaces :: (Path, Block) -> [Face]
leafFaces (_, Air) =
  []
leafFaces (path, Solid) =
  cubeFaces (pathCube path)

