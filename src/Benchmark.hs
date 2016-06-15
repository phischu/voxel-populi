module Main where

import Voxel (
  Cube(Cube), Side(..), volumeVoxels, unitVoxel)
import Grid (
  fromVoxels)

import Criterion (
  bgroup, bench, whnfIO)
import Criterion.Main (
  defaultMain)

import Linear (
  V3(V3), (*^), (^+^), (^-^),
  norm)


main :: IO ()
main = do
  let voxelsSmall = volumeVoxels 2 2 ball unitVoxel
      voxelsMedium = volumeVoxels 6 2 ball unitVoxel
      voxelsLarge = volumeVoxels 2 8 ball unitVoxel
  defaultMain [
    bgroup "Grid" [
      bench "fromVoxels-small" (whnfIO (fromVoxels 4 voxelsSmall)),
      bench "fromVoxels-medium" (whnfIO (fromVoxels 64 voxelsMedium)),
      bench "fromVoxels-large" (whnfIO (fromVoxels 64 voxelsLarge))]]

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

