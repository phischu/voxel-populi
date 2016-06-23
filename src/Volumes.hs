module Volumes where

import Voxel (
  Cube(Cube), Side(..))

import Linear (
  V3(V3), (*^), (^+^), (^-^),
  norm)

import Control.Applicative (liftA3)


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
  | size < 0.5 && all (<0) corners = Inside
  | size < 0.5 && all (>0) corners = Outside
  | otherwise = Border where
    corners = do
      d <- liftA3 V3 [0, size] [0, size] [0, size]
      return (value (position ^+^ d))
    value x = sum (fmap (\xi -> sin (omega * xi / (2 * pi))) x)
    omega = 70

