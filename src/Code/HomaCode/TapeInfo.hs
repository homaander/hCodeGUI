module Code.HomaCode.TapeInfo (TapeInfo(..)) where

import Code.HomaCode.Tape
import Code.HomaCode.Math
import Code.HomaCode.Code
import Code.HomaCode.Data

import Data.Maybe (mapMaybe, fromJust)
import Data.List (nub, intersect)



class Tape a => TapeInfo a where
  getSumsList   :: Int -> [a] -> [a] -> [[a]]
  getOfsetsSums :: Int -> [a] -> [a] ->  [a]  ->  [(Int,Int)]

  trapFinderLength :: HBase -> HRank -> [a] -> Int
  trapFinderOffset :: [a] -> [a] -> Int

instance TapeInfo HNum where
  getSumsList c aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. c]

  getOfsetsSums count aTape bTape resTape = mapMaybe check [0 .. count]
    where
      lid = getTapeId resTape
      check b_offset = if tapeId nsumd == lid 
                       then Just (b_offset, tapeAntiOffset nsumd)
                       else Nothing
        where
          nsum  = aTape ^+ codeN b_offset bTape
          nsumd = toTape nsum

  trapFinderLength base rank dat =  (100000 * fst (head $ filter (\(_, r) -> r == closest) nums_res)) + step
    where
      preset   = getPreset @HNum base rank (-100000)
      results  = take 10000 $ iterate (runPreset preset) dat

      trap     = codeNList 100000 dat
      closest  = head $ results `intersect` trap

      nums_res = zip [0..] results
      step     = fromJust $ findOffset dat closest

  trapFinderOffset a b = (100000 * fst (head $ filter (\(_, r) -> r == b) nums_res)) + step
    where
      preset   = getPreset @HNum (hBase $ head a) (length a) (-100000)
      results  = take 10000 $ iterate (runPreset preset) a

      nums_res = zip [0..] results
      step     = fromJust $ findOffset a b



-- "ANDREW" = 40356 + (100'000 x)
-- in 100k: "V2SZMU"
-- 142640356



-- import qualified Code.HomaCode as HC
-- import Code.HomaCode.Data

-- import Control.Parallel.Strategies
-- -- import Control.DeepSeq


-- import Data.List  (elemIndex)
-- import Data.Maybe (isJust)

-- -- Strats

-- preset2_5M :: [[HNumsL]]
-- preset2_5M = map HC.getHCT ["_DOD_5","DNQNI4","OQMVSH","DNVRU4","_ISU3H","54H4HG"]

-- getStarter :: [HNumsL] -> [[HNumsL]]
-- getStarter a = take 10 $ iterate (HC.runPreset preset2_5M) a


-- getTapeIdParallel :: [HNumsL] -> [Int] -> [HNumsL]
-- getTapeIdParallel dat c = minimum res
--   where
--     res = map (getTapeIdFromChank dat) c `using` parList rpar

-- getTapeIdFromChank :: [HNumsL] -> Int -> [HNumsL]
-- getTapeIdFromChank a n =
--   minimum $
--   take 10000 $
--   drop (10000 * n) $
--   iterate HC.code (HC.code a)


-- getOffsetParallel :: [HNumsL] -> [HNumsL] -> [Int] -> [(Int, Maybe Int)]
-- getOffsetParallel a b c = filter (\(_,g) -> isJust g) res
--   where
--     res = map (getOffsetChank a b) c `using` parList rdeepseq

-- getOffsetChank :: HC.Code a => a -> a -> Int -> (Int, Maybe Int)
-- getOffsetChank a b n =
--   (,) (10000 * n) $ elemIndex b $
--   take 10000 $
--   drop (10000 * n) $
--   iterate HC.code (HC.code a)