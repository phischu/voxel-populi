module Main where

import Voxel (sampleGrid, Cube(Cube), Side(..))

import Criterion (
  bgroup, bench, nfIO)
import Criterion.Main (
  defaultMain)

import Linear (
  V3(V3), (*^), (^+^), (^-^),
  norm)


main :: IO ()
main = defaultMain [
  bgroup "sampleGrid" [
    bench "sampleGrid-2-2" (nfIO (sampleGrid 2 2 ball)),
    bench "sampleGrid-6-2" (nfIO (sampleGrid 6 2 ball)),
    bench "sampleGrid-2-8" (nfIO (sampleGrid 2 8 ball))]]

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

