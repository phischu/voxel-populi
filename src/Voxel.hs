{-# LANGUAGE DeriveGeneric #-}
module Voxel where

import Linear (
  V3(V3), (*^), (^+^))

import Streaming.Prelude (
  Stream, Of)

import qualified Streaming.Prelude as S (
  map, filter, for, mapM, yield, each, foldM_)

import Data.Array.IO (
  IOArray, newArray, writeArray, readArray)

import Data.Foldable (for_)
import Control.Applicative (liftA3)


type Depth = Int
type Resolution = Int
type Location = V3 Int
data Address = Address Resolution Location

data Cube = Cube Float (V3 Float)
  deriving (Show, Eq, Ord)

data Side = Outside | Border | Inside
  deriving (Show, Eq, Ord)

data Grid a = Grid !Resolution !(IOArray Location a)


sampleGrid :: Depth -> Resolution -> (Cube -> Side) -> IO (Grid Bool)
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

fromAddresses :: Resolution -> Stream (Of Address) IO r -> IO (Grid Bool)
fromAddresses resolution =
  S.foldM_
    (\grid address -> setAddress grid address True)
    (emptyGrid resolution)
    return

emptyGrid :: Resolution -> IO (Grid Bool)
emptyGrid resolution = do
  let locationBounds = (V3 0 0 0, pure resolution - 1)
  values <- newArray locationBounds False
  return (Grid resolution values)

setAddress :: Grid Bool -> Address -> Bool -> IO (Grid Bool)
setAddress (Grid resolution values) address value = do
  for_ (addressLocations resolution address) (\location ->
    writeArray values location value)
  return (Grid resolution values)

visibleAddresses :: Grid Bool -> Stream (Of Address) IO ()
visibleAddresses grid =
  S.map fst (
    S.filter snd (
      getAddress grid unitAddress))

getAddress :: Grid Bool -> Address -> Stream (Of (Address,Bool)) IO ()
getAddress (Grid resolution values) address =
  S.mapM (\i -> do
    value <- readArray values i
    return (Address resolution i, value)) (
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

unitCubes :: Resolution -> [Address]
unitCubes resolution = do
  let locations = [0 .. resolution - 1]
  i <- liftA3 V3 locations locations locations
  return (Address resolution i)

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

