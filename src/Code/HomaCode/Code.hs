module Code.HomaCode.Code (Code(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Math
import Code.HomaCode.HData
import Control.Monad

class HData a => Code a where
  (-^>) :: [a] -> HCount -> [a]
  (<^-) :: [a] -> HCount -> [a]

  (*^>) :: [[a]] -> [a] -> [a]

  getPreset  :: HBase -> HRank -> HCount -> [[a]]
  execPreset :: [[a]] -> [a] -> [[a]]
  runPreset  :: [[a]] -> [a] ->  [a]

  code       :: [a] -> [a]
  codeN      :: HCount -> [a] ->  [a]
  codeNList  :: HCount -> [a] -> [[a]]
  codeR      :: [a] -> [a]

  decode  ::  [a] -> [a]
  decodeN ::  HCount -> [a] -> [a]

  findOffset :: [a] -> [a] -> Maybe Int
  findList   :: [a] -> [a] -> Maybe [[a]]


  -- default
  (-^>)  = flip codeN
  (<^-)  = flip decodeN

  (*^>)  = runPreset

  codeN     n hdata = iterate code hdata !! n
  codeNList n ihd   = take n $ iterate code (code ihd)

  codeR hdata = codeN (fromHData hdata) hdata

  decodeN n hdata = iterate decode hdata !! n


instance Code HNum where
  code d = reverse $ d ^- (d >^> 1)

  decode dat = reverse [foldl1 (^+) (dat <^< (a - 1)) | a <- [1 .. length dat]]

  findOffset [] _ = Nothing
  findOffset ihd hdata = do
      guard $ res /= maxlen
      pure res
    where
      res         = foldr finder 0 (codeNList maxlen ihd)
      finder he n = if he == hdata then 1 else n + 1
      maxlen      = hBase (head ihd) ^ length ihd


  findList ihd hdata = do
    off <- findOffset ihd hdata
    pure $ ihd : codeNList off ihd

  getPreset b n a | a <= 0    = map (decodeN (1 - a)) preset
                  | otherwise = map (codeN   (a - 1)) preset
    where
      st = setRank b n [neg $ HN b 1, HN b 1]
      preset = map (st <^<) [0 .. n - 1]

  -- runPreset preset dat  = map (foldl1 (^+) . (^* dat)) preset
  runPreset preset dat  = map (foldl1 (^+)) $ execPreset preset dat

  execPreset preset dat = map (^* dat) preset


-- >>> findOffset [HN 10 1, HN 10 2] [HN 10 1, HN 10 1]
-- Just 1

-- >>> [HN 10 1, HN 10 2] <^< 1
-- [HN {hBase = 10, hVal = 2},HN {hBase = 10, hVal = 0}]

-- >>> [HN 10 2, HN 10 4] ^- [HN 10 9, HN 10 2]
-- [HN {hBase = 10, hVal = 3},HN {hBase = 10, hVal = 2}]

-- >>> neg $ HN 10 0
-- HN {hBase = 10, hVal = 0}

-- >>> zipWith (^-) [HN 10 2, HN 10 4] ([HN 10 9, HN 10 2] >^> 1)
-- [HN {hBase = 10, hVal = 2},HN {hBase = 10, hVal = 5}]

-- >>> [HN 10 2, HN 10 4] ^- ([HN 10 9, HN 10 2] >^> 1)
-- [HN {hBase = 10, hVal = 2},HN {hBase = 10, hVal = 5}]

-- >>> code @HNum [HN 10 1, HN 10 2]
-- [HN {hBase = 10, hVal = 1},HN {hBase = 10, hVal = 1}]

-- >>> map showHCode $ getPreset @HNum 20 5 666
-- ["8HB02","HJHD2","BH1JD","0DJ1A","22DAI"]
