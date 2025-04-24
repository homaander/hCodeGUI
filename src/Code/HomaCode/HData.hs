module Code.HomaCode.HData (HData(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Math

class Math a => HData a where
  (<^<) :: [a] -> HCount -> [a]
  (>^>) :: [a] -> HCount -> [a]
  setRank :: HBase -> HRank -> [a] -> [a]

  fromHData   :: [a] -> Int
  toHData     :: HBase -> Int -> [a]
  toHDataN    :: HBase -> HRank -> Int -> [a]


  setBase :: HBase -> Int -> [a] -> [a]
  resetBase :: HBase -> [a] -> [a]

  -- default
  toHDataN base rank dat = setRank base rank $ toHData base dat

  setBase base rank dat = toHDataN base rank $ fromHData dat


instance HData HNum where
  [] <^< _ = error "Empty HData"
  dat@(fd:_) <^< n = drop n dat <> replicate n (HN (hBase fd) 0)

  [] >^> _ = error "Empty HData"
  dat@(fd:_) >^> n =  replicate n (HN (hBase fd) 0) <> take (length dat - n) dat

  setRank base rank dat = replicate (rank - length dat) (HN base 0) <> dat

  fromHData [] = 0
  fromHData hdata@(hf:_) = sum $ zipWith (*) (map hVal hdata) powArr
    where
      powArr = map (hBase hf ^) $ reverse [0 .. length hdata - 1]

  toHData base num = map (HN base . (`mod` base) . div num) powArr
    where
      powArr = map (base ^) $ reverse [0 .. len - 1]
      len    = ceiling @Double @Int $ logBase (fromIntegral base) (fromIntegral num + 0.1)

  resetBase base = map (\(HN _ v) -> HN base v)

