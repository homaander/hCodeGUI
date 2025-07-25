module Code.HomaCode.TapeInfo (
    TapeInfo(..)
  , TapeInfoParallel(..)
) where

import Code.HomaCode.Tape
import Code.HomaCode.Math
import Code.HomaCode.Code
import Code.HomaCode.Data

import Data.Maybe (mapMaybe, fromJust)
import Data.List (nub, intersect)

import Control.Parallel.Strategies
-- import Control.DeepSeq


class Tape a => TapeInfo a where
  getSumsList   :: Int -> [a] -> [a] -> [[a]]
  getOfsetsSums :: Int -> [a] -> [a] ->  [a]  ->  [(Int,Int)]

  trapFinderLength :: [a] -> Int
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

  trapFinderLength dat = trapFinderOffset dat dat

  trapFinderOffset [] _  = error "Empty HData"
  trapFinderOffset _  [] = error "Empty HData"
  trapFinderOffset a@(ha:_) b = (steps * fst res) + step
    where
      steps    = 100000

      -- Base 10 Rank 8
      -- preset = [
      --            [HN 10 0,HN 10 6,HN 10 8,HN 10 0,HN 10 3,HN 10 3,HN 10 9,HN 10 3]
      --          , [HN 10 6,HN 10 8,HN 10 6,HN 10 1,HN 10 3,HN 10 2,HN 10 6,HN 10 2]
      --          , [HN 10 8,HN 10 6,HN 10 1,HN 10 9,HN 10 0,HN 10 6,HN 10 5,HN 10 5]
      --          , [HN 10 0,HN 10 1,HN 10 9,HN 10 0,HN 10 2,HN 10 3,HN 10 5,HN 10 8]
      --          , [HN 10 3,HN 10 3,HN 10 0,HN 10 2,HN 10 3,HN 10 1,HN 10 6,HN 10 8]
      --          , [HN 10 3,HN 10 2,HN 10 6,HN 10 3,HN 10 1,HN 10 6,HN 10 4,HN 10 6]
      --          , [HN 10 9,HN 10 6,HN 10 5,HN 10 5,HN 10 6,HN 10 4,HN 10 6,HN 10 2]
      --          , [HN 10 3,HN 10 2,HN 10 5,HN 10 8,HN 10 8,HN 10 6,HN 10 2,HN 10 2]
      --          ]

      preset   = getPreset @HNum (hBase ha) (length a) (-steps)
      results  = take 10000 $ iterate (runPreset preset) a

      trap     = codeNList steps b
      closest  = case results `intersect` trap of
                   [] -> error "CC"
                   (x:_) -> x

      nums_res = zip [0..] results
      step     = fromJust $ findOffset b closest

      res = case filter (\(_, r) -> r == closest) nums_res of
              [] -> error "AA"
              (x:_) -> x



class TapeInfo a => TapeInfoParallel a where
  getTapeIdParallel :: [a] -> [Int] -> [a]
  getTapeIdFromChank :: [a] -> Int -> [a]

instance TapeInfoParallel HNum where
  getTapeIdParallel dat c = minimum res
    where
      res = map (getTapeIdFromChank dat) c `using` parList rpar
  getTapeIdFromChank a n =
    minimum $
    take 10000 $
    drop (10000 * n) $
    iterate code (code a)



-- "ANDREW" = 40356 + (100'000 x)
-- in 100k: "V2SZMU"
-- 142640356



-- import qualified Code.HomaCode as HC
-- import Code.HomaCode.Data

-- import Control.Parallel.Strategies
-- import Control.DeepSeq


-- import Data.List  (elemIndex)
-- import Data.Maybe (isJust)

-- preset2_5M :: [[HNumsL]]
-- preset2_5M = map HC.getHCT ["_DOD_5","DNQNI4","OQMVSH","DNVRU4","_ISU3H","54H4HG"]

-- getStarter :: [HNumsL] -> [[HNumsL]]
-- getStarter a = take 10 $ iterate (HC.runPreset preset2_5M) a





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