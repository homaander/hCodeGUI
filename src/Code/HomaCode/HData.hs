module Code.HomaCode.HData (HData(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Math

class Math a => HData a where
  (<^<) :: [a] -> HCount -> [a]
  (>^>) :: [a] -> HCount -> [a]

  fromHData   :: [a] -> Int
  toHData     :: HBase -> Int -> [a]
  toHDataN    :: HBase -> HRank -> Int -> [a]

  setRank :: HBase -> HRank -> [a] -> [a]

  setBase :: HBase -> Int -> [a] -> [a]
  resetBase :: HBase -> [a] -> [a]

  -- default
  toHDataN base rank dat = setRank base rank $ toHData base dat

  setBase base rank dat = toHDataN base rank $ fromHData dat


instance HData HNum where
  dat <^< n = drop n dat <> replicate n (HN (hBase $ head dat) 0)

  dat >^> n =  replicate n (HN (hBase $ head dat) 0) <> take (length dat - n) dat

  fromHData [] = 0
  fromHData hdata@(hf:_) = sum $ zipWith (*) (map hVal hdata) powArr
    where
      powArr = map (hBase hf ^) $ reverse [0 .. length hdata - 1]

  toHData base num = map (HN base . (`mod` base) . div num) powArr
    where
      powArr = map (base ^) $ reverse [0 .. len - 1]
      len    = ceiling @Double @Int $ logBase (fromIntegral base) (fromIntegral num + 0.1)

  setRank base rank dat = replicate (rank - length dat) (HN base 0) <> dat

  resetBase base = map (\(HN _ v) -> HN base v)

