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
  -- Get Tape Id from (a + b_offset)
  getSumsList c aTape bTape = nub $ map (\n -> getTapeId (aTape ^+ codeN n bTape)) [0 .. c]

  -- Get (b_offset, c_anti_offset) from (a + b_offset = c_offset)
  getOfsetsSums count aTape bTape resTape = mapMaybe check [0 .. count]
    where
      lid = getTapeId resTape
      check b_offset = if tapeId nsumd == lid 
                       then Just (b_offset, tapeAntiOffset nsumd)
                       else Nothing
        where
          nsum  = aTape ^+ codeN b_offset bTape
          nsumd = toTape nsum

  -- Get length tape for big tapes (base: 37, rank: 6)
  trapFinderLength base rank dat =  (100000 * fst (head $ filter (\(_, r) -> r == closest) nums_res)) + step
    where
      preset   = getPreset @HNum base rank (-100000)
      results  = take 10000 $ iterate (runPreset preset) dat

      trap     = codeNList 100000 dat
      closest  = head $ results `intersect` trap

      nums_res = zip [0..] results
      step     = fromJust $ findOffset dat closest

  -- Get big offset between code
  trapFinderOffset a b = (100000 * fst (head $ filter (\(_, r) -> r == b) nums_res)) + step
    where
      preset   = getPreset @HNum (hBase $ head a) (length a) (-100000)
      results  = take 10000 $ iterate (runPreset preset) a

      nums_res = zip [0..] results
      step     = fromJust $ findOffset a b


-- "ANDREW" = 40356 + (100'000 x)
-- in 100k: "V2SZMU"
-- 142640356