module Code.HomaCode.Parallel.TapeInfo ()
-- (
--    getTapeIdParallel
--  , getOffsetParallel
--  , getStarter

--  , preset2_5M
-- ) 
where

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


