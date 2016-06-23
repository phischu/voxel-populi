module Main where

import Voxel (
  Cube, Side(..), Depth, unitPath)
import Octree (
  fromVolume, enumerate)
import Volumes (
  ball, cave)

import Test.Hspec (
  SpecWith, hspec, describe, it, shouldBe)


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
    depthLeafs cave 8 560148))

depthLeafs :: (Cube -> Side) -> Depth -> Int -> SpecWith ()
depthLeafs volume depth n = it ("Depth: " ++ show depth) (do
  length (enumerate (fromVolume depth volume unitPath)) `shouldBe` n)

