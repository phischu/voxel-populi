module Camera where

import Linear (
  V2(V2), V3(V3), (^+^), (^-^),
  unit, _y, _z,
  dot, cross, normalize, project,
  M44, mkTransformation,
  Quaternion(Quaternion), rotate)

import Graphics.GL

data Camera = Camera {
  _cameraPosition :: V3 GLfloat,
  _cameraOrientation :: Quaternion GLfloat }
    deriving (Show,Eq,Ord)

lookAt :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> Camera
lookAt eye center up = Camera eye orientation where
  orientation = upOrientation * centerOrientation
  centerOrientation = quaternionBetweenVectors (negate (unit _z)) direction
  upOrientation = quaternionBetweenVectors rotatedUp wantedUp
  rotatedUp = rotate centerOrientation (unit _y)
  wantedUp = up ^-^ project direction up
  direction = normalize (center ^-^ eye)

quaternionBetweenVectors :: V3 GLfloat -> V3 GLfloat -> Quaternion GLfloat
quaternionBetweenVectors x y =
  normalize (1 + Quaternion (dot x y) (cross x y))

fly :: V3 GLfloat -> Camera -> Camera
fly direction (Camera position orientation) =
  Camera (position ^+^ rotate orientation direction) orientation

pan :: V2 GLfloat -> Camera -> Camera
pan (V2 horizontal vertical) (Camera position orientation) =
  Camera position (orientation * rotation) where
    rotation = quaternionBetweenVectors negativeZ direction
    negativeZ = V3 0 0 (-1)
    direction = V3 horizontal vertical (-1)

cameraMatrix :: Camera -> M44 GLfloat
cameraMatrix (Camera position orientation) =
  mkTransformation inverseOrientation inversePosition where
    inverseOrientation = 1 / orientation
    inversePosition = rotate inverseOrientation (negate position)


