module Main where

import Voxel (
  Path(Path), Cube(Cube), Side(..), unitPath,
  Block(Air, Solid))
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
      bench "Grid" (whnfIO (Grid.fromVolume 4 ball unitPath)),
      bench "Octree" (nf (Octree.fromVolume 2 ball) unitPath)],
    bgroup "medium" [
      bench "Grid" (whnfIO (Grid.fromVolume 16 ball unitPath)),
      bench "Octree" (nf (Octree.fromVolume 4 ball) unitPath)],
    bgroup "large" [
      bench "Grid" (whnfIO (Grid.fromVolume 64 ball unitPath)),
      bench "Octree" (nf (Octree.fromVolume 6 ball) unitPath)]],
  bgroup "setVoxel" [
    env (Grid.fromVolume 64 ball unitPath) (\grid ->
      bench "Grid" (whnfIO (Grid.setVoxel grid voxelToSet True))),
    env (return (Octree.fromVolume 6 ball unitPath)) (\octree ->
      bench "Octree" (nf (Octree.setVoxel octree voxelToSet) Solid))],
  bgroup "enumerate" [
    env (Grid.fromVolume 64 ball unitPath) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.enumerate grid)))),
    env (return (Octree.fromVolume 6 ball unitPath)) (\octree ->
      bench "Octree" (whnfIO (forceStream (S.each (Octree.enumerate octree)))))],
  bgroup "toMesh" [
    env (Grid.fromVolume 64 ball unitPath) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.toMesh grid)))),
    env (return (Octree.fromVolume 6 ball unitPath)) (\octree ->
      bench "Octree" (whnfIO (forceStream (Octree.toMesh octree))))],
  env (return (Octree.fromVolume 6 ball unitPath)) (\octree ->
      bench "neighbours" (whnfIO (forceStream (S.each (
        Octree.enumerate (Octree.neighbours octree (Octree.Full Air)))))))]


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


voxelToSet :: Path
voxelToSet = Path 16 (V3 1 1 1)


voxelToGet :: Path
voxelToGet = Path 4 (V3 1 1 1)

