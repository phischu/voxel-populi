module Main where

import Voxel (
  Voxel(Voxel), Cube(Cube), Side(..), volumeVoxels, unitVoxel)
import Grid (
  fromVoxels, setVoxel, getVoxels)

import qualified Streaming.Prelude as S (
  effects)

import Criterion (
  bgroup, bench, env, whnfIO)
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
      bgroup "fromVoxels" [
        bench "fromVoxels-small" (whnfIO (fromVoxels 4 voxelsSmall)),
        bench "fromVoxels-medium" (whnfIO (fromVoxels 64 voxelsMedium)),
        bench "fromVoxels-large" (whnfIO (fromVoxels 64 voxelsLarge))],
      env (fromVoxels 64 voxelsMedium) (\grid ->
          bench "setVoxel" (whnfIO (setVoxel grid voxelToSet True))),
      env (fromVoxels 64 voxelsMedium) (\grid ->
          bench "getVoxels" (whnfIO (S.effects (getVoxels grid voxelToGet))))]]

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

voxelToSet :: Voxel
voxelToSet = Voxel 16 (V3 1 1 1)

voxelToGet :: Voxel
voxelToGet = Voxel 4 (V3 1 1 1)

