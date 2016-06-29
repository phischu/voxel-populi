module Main where

import Voxel (
  Cube, Side(..), Depth, Resolution, rootPath)
import qualified Octree (
  fromVolume, enumerate, stupidMesh, naiveMesh)
import qualified Grid (
  fromVolume, stupidMesh, naiveMesh)
import Volumes (
  ball, cave)

import Test.Hspec (
  SpecWith, hspec, describe, it, shouldBe, shouldReturn)

import qualified Streaming.Prelude as S (
  length_)


main :: IO ()
main = hspec (do

  describe "Octree.fromVolume ball" (do
    depthLeafs ball 1 1
    depthLeafs ball 2 64
    depthLeafs ball 3 232
    depthLeafs ball 4 1408
    depthLeafs ball 5 5944
    depthLeafs ball 6 24760
    depthLeafs ball 7 98848
    depthLeafs ball 8 408808)

  describe "Octree.fromVolume cave" (do
    depthLeafs cave 1 1
    depthLeafs cave 2 1
    depthLeafs cave 3 225
    depthLeafs cave 4 1443
    depthLeafs cave 5 6924
    depthLeafs cave 6 33860
    depthLeafs cave 7 140260
    depthLeafs cave 8 560148)

  describe "Grid.stupidMesh ball" (do
    resolutionFacesStupidMesh ball 8 816
    resolutionFacesStupidMesh ball 16 9408
    resolutionFacesStupidMesh ball 32 87552
    resolutionFacesStupidMesh ball 64 759312
    resolutionFacesStupidMesh ball 128 6324768
    resolutionFacesStupidMesh ball 256 51648096)

  describe "Octree.stupidMesh ball" (do
    depthFacesStupidMesh ball 3 480
    depthFacesStupidMesh ball 4 3360
    depthFacesStupidMesh ball 5 15648
    depthFacesStupidMesh ball 6 68496
    depthFacesStupidMesh ball 7 295584
    depthFacesStupidMesh ball 8 1201392)

  describe "Grid.naiveMesh ball" (do
    resolutionFacesNaiveMesh ball 8 192
    resolutionFacesNaiveMesh ball 16 984
    resolutionFacesNaiveMesh ball 32 4344
    resolutionFacesNaiveMesh ball 64 18288
    resolutionFacesNaiveMesh ball 128 75192
    resolutionFacesNaiveMesh ball 256 304608)

  describe "Octree.naiveMesh ball" (do
    depthFacesNaiveMesh ball 3 192
    depthFacesNaiveMesh ball 4 984
    depthFacesNaiveMesh ball 5 4056
    depthFacesNaiveMesh ball 6 17064
    depthFacesNaiveMesh ball 7 70872
    depthFacesNaiveMesh ball 8 287616))

depthLeafs :: (Cube -> Side) -> Depth -> Int -> SpecWith ()
depthLeafs volume depth n =
  it ("Depth: " ++ show depth) (do
    let octree = Octree.fromVolume depth volume rootPath
    length (Octree.enumerate octree) `shouldBe` n)

resolutionFacesStupidMesh :: (Cube -> Side) -> Resolution -> Int -> SpecWith ()
resolutionFacesStupidMesh volume resolution n =
  it ("Resolution: " ++ show resolution) ((do
    grid <- Grid.fromVolume resolution volume rootPath
    S.length_ (Grid.stupidMesh grid)) `shouldReturn` n)

depthFacesStupidMesh :: (Cube -> Side) -> Depth -> Int -> SpecWith ()
depthFacesStupidMesh volume depth n =
  it ("Depth: " ++ show depth) (do
    let octree = Octree.fromVolume depth volume rootPath
    length (Octree.stupidMesh octree) `shouldBe` n)

resolutionFacesNaiveMesh :: (Cube -> Side) -> Resolution -> Int -> SpecWith ()
resolutionFacesNaiveMesh volume resolution n =
  it ("Resolution: " ++ show resolution) ((do
    grid <- Grid.fromVolume resolution volume rootPath
    S.length_ (Grid.naiveMesh grid)) `shouldReturn` n)

depthFacesNaiveMesh :: (Cube -> Side) -> Depth -> Int -> SpecWith ()
depthFacesNaiveMesh volume depth n =
  it ("Depth: " ++ show depth) (do
    let octree = Octree.fromVolume depth volume rootPath
    length (Octree.naiveMesh octree) `shouldBe` n)

