module Main where

import Voxel (
  Cube(Cube), Side(..), Depth, unitPath)
import Octree (
  fromVolume, enumerate)

import Test.Hspec (
  SpecWith, hspec, describe, it, shouldBe)

import Linear (
  V3(V3), (^+^), (^-^), (*^), norm)

import Control.Applicative (
  liftA3)


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
    depthLeafs cave 3 1
    depthLeafs cave 4 1485
    depthLeafs cave 5 7050
    depthLeafs cave 6 34112
    depthLeafs cave 7 140680
    depthLeafs cave 8 560820))

depthLeafs :: (Cube -> Side) -> Depth -> Int -> SpecWith ()
depthLeafs volume depth n = it ("Depth: " ++ show depth) (do
  length (enumerate (fromVolume depth volume unitPath)) `shouldBe` n)


ball :: Cube -> Side
ball (Cube size position)
  | distance < circleRadius - cubeRadius = Inside
  | distance > circleRadius + cubeRadius = Outside
  | otherwise = Border where
    circleCenter = V3 0.5 0.5 0.5
    circleRadius = 0.5
    cubeCenter = position ^+^ halfSize
    cubeRadius = norm halfSize
    halfSize = 0.5 *^ (V3 size size size)
    distance = norm (circleCenter ^-^ cubeCenter)

cave :: Cube -> Side
cave (Cube size position)
  | size < 0.1 && all (<0) corners = Inside
  | size > 0.1 && all (>0) corners = Outside
  | otherwise = Border where
    corners = do
      d <- liftA3 V3 [0, size] [0, size] [0, size]
      return (value (position ^+^ d))
    value x = sum (fmap (\xi -> sin (omega * xi / (2 * pi))) x)
    omega = 70

