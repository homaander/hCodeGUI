module Code.HomaCode.Data (
  HTape (..),

  HNum (..),
  toLetter,
  showHCode,
  fromLetter,

  HBase,
  HVal,
  HCount,
  HRank,

  hcodeAlfebet
) where

import Data.List  ( elemIndex )
import Data.Maybe ( fromMaybe )

hcodeAlfebet :: String
hcodeAlfebet  = "0123456789AB"
             <> "CDEFGHIJKLMN"
             <> "OPQRSTUVWXYZ_"

type HBase  = Int
type HVal   = Int
type HCount = Int
type HRank  = Int


data HNum = HN { 
  hBase :: HBase,
  hVal  :: HVal 
  }
    deriving (Show, Eq)

instance Ord HNum where
  (HN _ a) <= (HN _ b) = a <= b

instance Read HNum where
  readsPrec _ [] = []
  readsPrec n (x:xs) = (fromLetter 10 x, xs) : readsPrec n xs

toLetter :: HNum -> Char
toLetter (HN _ a) = hcodeAlfebet !! a

fromLetter :: HBase -> Char -> HNum
fromLetter base a = HN base $ fromMaybe 0 $
               elemIndex a $
               take base hcodeAlfebet

showHCode :: [HNum] -> String
showHCode a = mconcat $ map (pure . toLetter) a



data HTape hdata = HTape {
  tapeId         :: hdata,
  tapeOffset     :: Int,
  tapeAntiOffset :: Int,
  tapeLength     :: Int
  }
    deriving Show