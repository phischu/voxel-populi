module Main where

import Voxel (
  Voxel(Voxel), Cube(Cube), Side(..), unitVoxel)
import qualified Grid (
  fromVolume, setVoxel, enumerate, toMesh)
import qualified Octree (
  fromVolume, setVoxel, enumerate, toMesh,
  neighbours, Octree(Full))

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  mapM_, each)

import Criterion (
  bgroup, bench, env, whnfIO, nf)
import Criterion.Main (
  defaultMain)

import Linear (
  V3(V3), (*^), (^+^), (^-^),
  norm)

import Control.DeepSeq (
  NFData, force)
import Control.Exception (
  evaluate)


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
    env (Grid.fromVolume 64 ball unitVoxel) (\grid ->
      bench "Grid" (whnfIO (Grid.setVoxel grid voxelToSet True))),
    env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
      bench "Octree" (nf (Octree.setVoxel octree voxelToSet) True))],
  bgroup "enumerate" [
    env (Grid.fromVolume 64 ball unitVoxel) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.enumerate grid)))),
    env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
      bench "Octree" (whnfIO (forceStream (S.each (Octree.enumerate octree)))))],
  bgroup "toMesh" [
    env (Grid.fromVolume 64 ball unitVoxel) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.toMesh grid)))),
    env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
      bench "Octree" (whnfIO (forceStream (Octree.toMesh octree))))],
  env (return (Octree.fromVolume 6 ball unitVoxel)) (\octree ->
      bench "neighbours" (whnfIO (forceStream (S.each (
        Octree.enumerate (Octree.neighbours octree (Octree.Full False)))))))]


forceStream :: (NFData a) => Stream (Of a) IO r -> IO r
forceStream = S.mapM_ (evaluate . force)

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

