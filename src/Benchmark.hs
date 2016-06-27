module Main where

import Voxel (
  Path(Path), Block(Air, Solid), rootPath)
import qualified Grid (
  fromVolume, setVoxel, enumerate, stupidMesh)
import qualified Octree (
  fromVolume, setVoxel, enumerate, stupidMesh, naiveMesh,
  neighbours, Octree(Full))
import Volumes (
  ball)

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  mapM_, each)

import Criterion (
  bgroup, bench, env, whnfIO, nf)
import Criterion.Main (
  defaultMain)

import Linear (
  V3(V3))

import Control.DeepSeq (
  NFData, force)
import Control.Exception (
  evaluate)


main :: IO ()
main = defaultMain [
  bgroup "fromVolume" [
    bgroup "small" [
      bench "Grid" (whnfIO (Grid.fromVolume 4 ball rootPath)),
      bench "Octree" (nf (Octree.fromVolume 2 ball) rootPath)],
    bgroup "medium" [
      bench "Grid" (whnfIO (Grid.fromVolume 16 ball rootPath)),
      bench "Octree" (nf (Octree.fromVolume 4 ball) rootPath)],
    bgroup "large" [
      bench "Grid" (whnfIO (Grid.fromVolume 64 ball rootPath)),
      bench "Octree" (nf (Octree.fromVolume 6 ball) rootPath)]],
  bgroup "setVoxel" [
    env (Grid.fromVolume 64 ball rootPath) (\grid ->
      bench "Grid" (whnfIO (Grid.setVoxel grid voxelToSet True))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree" (nf (Octree.setVoxel octree voxelToSet) Solid))],
  bgroup "enumerate" [
    env (Grid.fromVolume 64 ball rootPath) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.enumerate grid)))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree" (whnfIO (forceStream (S.each (Octree.enumerate octree)))))],
  bgroup "meshing" [
    env (Grid.fromVolume 64 ball rootPath) (\grid ->
      bench "Grid.stupidMesh" (whnfIO (forceStream (Grid.stupidMesh grid)))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree.stupidMesh" (whnfIO (forceStream (S.each (Octree.stupidMesh octree))))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree.naiveMesh" (whnfIO (forceStream (S.each (Octree.naiveMesh octree)))))],
  env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "neighbours" (whnfIO (forceStream (S.each (
        Octree.enumerate (Octree.neighbours octree (Octree.Full Air)))))))]


forceStream :: (NFData a) => Stream (Of a) IO r -> IO r
forceStream = S.mapM_ (evaluate . force)

voxelToSet :: Path
voxelToSet = Path 16 (V3 1 1 1)

voxelToGet :: Path
voxelToGet = Path 4 (V3 1 1 1)

