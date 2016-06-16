module Main where

import Voxel (
  Voxel(Voxel), Cube(Cube), Side(..), unitVoxel)
import qualified Grid (
  fromVolume, setVoxel, getVoxels)
import qualified Octree (
  fromVolume, setVoxel, getVoxels)

import qualified Streaming.Prelude as S (
  effects)

import Criterion (
  bgroup, bench, env, whnfIO, nf)
import Criterion.Main (
  defaultMain)

import Linear (
  V3(V3), (*^), (^+^), (^-^),
  norm)


main :: IO ()
main = defaultMain [
  bgroup "fromVolume" [
    bgroup "small" [
      bench "Grid" (whnfIO (Grid.fromVolume 4 ball unitVoxel)),
      bench "Octree" (nf (Octree.fromVolume 2 ball) unitVoxel)],
    bgroup "medium" [
      bench "Grid" (whnfIO (Grid.fromVolume 16 ball unitVoxel)),
      bench "Octree" (nf (Octree.fromVolume 4 ball) unitVoxel)],
    bgroup "large" [
      bench "Grid" (whnfIO (Grid.fromVolume 64 ball unitVoxel)),
      bench "Octree" (nf (Octree.fromVolume 6 ball) unitVoxel)]],
  bgroup "setVoxel" [
    bgroup "Grid" [
      env (Grid.fromVolume 64 ball unitVoxel) (\grid ->
        bench "setVoxel" (whnfIO (Grid.setVoxel grid voxelToSet True)))],
    bgroup "Octree" [
      env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
        bench "setVoxel" (nf (Octree.setVoxel octree voxelToSet) True))]],
  bgroup "getVoxel" [
    bgroup "Grid" [
      env (Grid.fromVolume 64 ball unitVoxel) (\grid ->
        bench "getVoxels" (whnfIO (S.effects (Grid.getVoxels grid voxelToGet))))],
    bgroup "Octree" [
      env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
        bench "getVoxels" (nf (Octree.getVoxels octree) voxelToGet))]]]


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

