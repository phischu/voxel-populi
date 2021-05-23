module Main where

import Voxel (
  Path(Path), Block(Air, Solid), rootPath, Resolution, Depth)
import qualified Grid (
  fromVolume, setVoxel, enumerate, stupidMesh, naiveMesh)
import qualified Octree (
  fromVolume, setVoxel, enumerate, stupidMesh, naiveMesh,
  neighbour, Octree(Full))
import Volumes (
  ball)

import Streaming (
  Stream, Of)
import qualified Streaming.Prelude as S (
  mapM_, each)

import Criterion (
  Benchmark, bgroup, bench, env, whnfIO, nf)
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
      bench "Grid" (whnfIO (Grid.setVoxel grid voxelToSet Solid))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree" (nf (Octree.setVoxel octree voxelToSet) Solid))],
  bgroup "enumerate" [
    env (Grid.fromVolume 64 ball rootPath) (\grid ->
      bench "Grid" (whnfIO (forceStream (Grid.enumerate grid)))),
    env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "Octree" (whnfIO (forceStream (S.each (Octree.enumerate octree)))))],
  bgroup "meshing" [
    bgroup "Grid.stupidMesh" [
      stupidMeshGrid 8,
      stupidMeshGrid 16,
      stupidMeshGrid 32,
      stupidMeshGrid 64,
      stupidMeshGrid 128,
      stupidMeshGrid 256],
    bgroup "Octree.stupidMesh" [
      stupidMeshOctree 3,
      stupidMeshOctree 4,
      stupidMeshOctree 5,
      stupidMeshOctree 6,
      stupidMeshOctree 7,
      stupidMeshOctree 8],
    bgroup "Grid.naiveMesh" [
      naiveMeshGrid 8,
      naiveMeshGrid 16,
      naiveMeshGrid 32,
      naiveMeshGrid 64,
      naiveMeshGrid 128,
      naiveMeshGrid 256],
    bgroup "Octree.naiveMesh" [
      naiveMeshOctree 3,
      naiveMeshOctree 4,
      naiveMeshOctree 5,
      naiveMeshOctree 6,
      naiveMeshOctree 7,
      naiveMeshOctree 8]],
  env (return (Octree.fromVolume 6 ball rootPath)) (\octree ->
      bench "neighbour" (whnfIO (forceStream (S.each (
        Octree.enumerate (Octree.neighbour octree (Octree.Full Air)))))))]


forceStream :: (NFData a) => Stream (Of a) IO r -> IO r
forceStream = S.mapM_ (evaluate . force)

voxelToSet :: Path
voxelToSet = Path 16 (V3 1 1 1)

voxelToGet :: Path
voxelToGet = Path 4 (V3 1 1 1)

stupidMeshGrid :: Resolution -> Benchmark
stupidMeshGrid resolution =
  env (Grid.fromVolume resolution ball rootPath) (\grid ->
    bench (show resolution) (whnfIO (
      forceStream (Grid.stupidMesh grid))))

naiveMeshGrid :: Resolution -> Benchmark
naiveMeshGrid resolution =
  env (Grid.fromVolume resolution ball rootPath) (\grid ->
    bench (show resolution) (whnfIO (
      forceStream (Grid.naiveMesh grid))))

stupidMeshOctree :: Depth -> Benchmark
stupidMeshOctree depth =
  env (return (Octree.fromVolume depth ball rootPath)) (\octree ->
    bench (show depth) (nf Octree.stupidMesh octree))

naiveMeshOctree :: Depth -> Benchmark
naiveMeshOctree depth =
  env (return (Octree.fromVolume depth ball rootPath)) (\octree ->
    bench (show depth) (nf Octree.naiveMesh octree))

