module Voxel where

import Graphics.GL

import Linear (
  V3(V3), (*^), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, filter, for, yield, each, fold_)

import Data.Array (
  Array, listArray, (!), (//), array, range)

import Control.Applicative (liftA3)


type Depth = Int
type Resolution = Int
type Location = V3 Int
data Address = Address Resolution Location

data Cube = Cube GLfloat (V3 GLfloat)
  deriving (Show, Eq, Ord)

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

data Grid a = Grid Resolution (Array Location a)


sampleGrid :: (Monad m) => Depth -> Resolution -> (Cube -> Side) -> m (Grid Bool)
sampleGrid depth resolution volume =
  fromAddresses (resolution ^ depth) (
    sampleAddresses depth resolution volume unitAddress)

sampleAddresses :: (Monad m) => Depth -> Resolution -> (Cube -> Side) -> Address -> Stream (Of Address) m ()
sampleAddresses depth resolution volume address
  | depth < 0 = return ()
  | otherwise = case volume (addressCube address) of
    Outside -> return ()
    Inside -> S.yield address
    Border -> S.for (childAddresses resolution address) (\childAddress ->
      sampleAddresses (depth - 1) resolution volume childAddress)

fromAddresses :: (Monad m) => Resolution -> Stream (Of Address) m r -> m (Grid Bool)
fromAddresses resolution =
  S.fold_
    (\grid address -> setAddress grid address True)
    (emptyGrid resolution)
    id

emptyGrid :: Resolution -> Grid Bool
emptyGrid resolution =
  Grid resolution (listArray locationBounds (repeat False)) where
    locationBounds = (V3 0 0 0, pure resolution - 1)

setAddress :: Grid Bool -> Address -> Bool -> Grid Bool
setAddress (Grid resolution values) address value =
  Grid resolution (values // updates) where
    updates = zip (addressLocations resolution address) (repeat value)

visibleAddresses :: (Monad m) => Grid Bool -> Stream (Of Address) m ()
visibleAddresses grid =
  S.map fst (
    S.filter snd (
      getAddress grid unitAddress))

getAddress :: (Monad m) => Grid Bool -> Address -> Stream (Of (Address,Bool)) m ()
getAddress (Grid resolution values) address =
  S.map (\i -> (Address resolution i, values ! i)) (
    S.each (addressLocations resolution address))

addressLocations :: Resolution -> Address -> [Location]
addressLocations resolution (Address addressResolution addressLocation) = do
    let relativeResolution =
          resolution `div` addressResolution
        relativeLocation =
          fmap (\x -> x * resolution `div` addressResolution) addressLocation
        locations = [0 .. relativeResolution - 1]
    i <- liftA3 V3 locations locations locations
    return (relativeLocation ^+^ i)

childAddresses :: (Monad m) => Resolution -> Address -> Stream (Of Address) m ()
childAddresses resolution address =
  S.map (relativeAddress address) (S.each (unitCubes resolution))

unitCubes :: Resolution -> Array Location Address
unitCubes resolution = array locationBounds (zip locations addresses) where
  locationBounds = (V3 0 0 0, pure resolution - 1)
  locations = range locationBounds
  addresses = fmap (Address resolution) locations

unitAddress :: Address
unitAddress = Address 1 (V3 0 0 0)

relativeAddress :: Address -> Address -> Address
relativeAddress parentAddress childAddress =
  Address resolution location where
    resolution = parentResolution * childResolution
    location = childResolution *^ parentLocation ^+^ childLocation
    Address parentResolution parentLocation = parentAddress
    Address childResolution childLocation = childAddress

addressCube :: Address -> Cube
addressCube (Address resolution location) = Cube size position where
  size = recip (realToFrac resolution)
  position = size *^ fmap realToFrac location

