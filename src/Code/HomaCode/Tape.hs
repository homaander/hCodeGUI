module Code.HomaCode.Tape (Tape(..)) where

import Code.HomaCode.Data
import Code.HomaCode.Code


import Data.Maybe (fromJust)

class (Ord a, Code a) => Tape a where
  toTape   :: [a] -> HTape [a]
  fromTape :: HTape [a] -> [a]

  getTapeId     :: [a] -> [a]
  getTapeLength :: [a] -> Int
  getTapeList   :: [a] -> [[a]]

  -- default
  fromTape (HTape h n _ _) = codeN n h

  getTapeId  = minimum . getTapeList

  getTapeLength hdata = fromJust $ findOffset hdata hdata
  getTapeList   hdata = fromJust $ findList   hdata hdata


instance Tape HNum where
  toTape hdata = HTape hid offset (len - offset) len
    where
      offset  = if offset' /= len then offset' else 0
      offset' = fromJust $ findOffset hid hdata
      len     = getTapeLength hdata
      hid     = getTapeId     hdata

-- >>> toTape [HN 16 12,HN 16 2,HN 16 13,HN 16 3,HN 16 14,HN 16 4]
-- HTape {tapeId = [HN 16 0,HN 16 0,HN 16 7,HN 16 4,HN 16 5,HN 16 4], tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504}
