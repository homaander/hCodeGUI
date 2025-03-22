# hCodeGUI

***[Документация HCode](https://github.com/homaander/reference/blob/master/help/hcode/main.md)***

```haskell
import HomaCode

data HNum = HN { 
  hBase :: HBase,
  hVal  :: HVal 
  }
    deriving (Eq)

data HTape hdata = HTape {
  tapeId         :: hdata,
  tapeOffset     :: Int,
  tapeAntiOffset :: Int,
  tapeLength     :: Int
  }
    deriving Show

hcodeAlfebet :: String
hcodeAlfebet  = "0123456789AB"
             <> "CDEFGHIJKLMN"
             <> "OPQRSTUVWXYZ_"

type HBase  = Int
type HVal   = Int
type HCount = Int
type HRank  = Int

hn :: HBase -> [Int] -> [HNum]

fromLetter :: HBase -> Char -> HNum
toLetter   :: HNum  -> Char
showHCode  :: [HNum] -> String

class (Eq a, Show a) => Math a where
  -- Sum without offset
  (^+) :: a -> a -> a
  -- (Default) Substract without offset
  (^-) :: a -> a -> a
  -- Multiply without offset (a + a + ... + a: n times)
  (^*) :: a -> a -> a

  -- Negative value
  neg  :: a -> a



class Math a => HData a where
  -- Add n zeros to end and cut off start
  (<^<) :: [a] -> HCount -> [a]
  -- Add n zeros to start and cut off end
  (>^>) :: [a] -> HCount -> [a]
  -- Set length value, add zeros ti start
  setRank :: HBase -> HRank -> [a] -> [a]

  -- Value to notation 10 number
  fromHData   :: [a] -> Int
  -- Number in 10 notation to value
  toHData     :: HBase -> Int -> [a]
  -- Number in 10 notation to value lenght
  toHDataN    :: HBase -> HRank -> Int -> [a]

  -- Set base for any value and recalculate numbers
  setBase :: HBase -> Int -> [a] -> [a]
  -- Set base for any value without recalculate numbers
  resetBase :: HBase -> [a] -> [a]



class HData a => Code a where
  -- codeN aliace
  (-^>) :: [a] -> HCount -> [a]
  -- decodeN aliace
  (<^-) :: [a] -> HCount -> [a]

  -- runPreset aliace
  (*^>) :: [[a]] -> [a] -> [a]

  -- Get CodeMap for N times
  getPreset  :: HBase -> HRank -> HCount -> [[a]]
  -- Get mult from CodeMap
  execPreset :: [[a]] -> [a] -> [[a]]
  -- Code with CodeMap
  runPreset  :: [[a]] -> [a] ->  [a]

  -- Code 1 times
  code       :: [a] -> [a]
  -- Code N times
  codeN      :: HCount -> [a] ->  [a]
  -- Get N next codes
  codeNList  :: HCount -> [a] -> [[a]]
  -- Code value times
  codeR      :: [a] -> [a]

  -- Decode 1 times
  decode  ::  [a] -> [a]
  -- Decode 1 times
  decodeN ::  HCount -> [a] -> [a]

  -- Try find count code times between two codes
  findOffset :: [a] -> [a] -> Maybe Int
  -- Try find all steps of code between two codes
  findList   :: [a] -> [a] -> Maybe [[a]]



class (Ord a, Code a) => Tape a where
  -- Get full info code
  toTape   :: [a] -> HTape [a]
  -- Get only code
  fromTape :: HTape [a] -> [a]

  -- Minimum in code tape
  getTapeId     :: [a] -> [a]
  getTapeLength :: [a] -> Int
  -- All codes in tape
  getTapeList   :: [a] -> [[a]]



class Tape a => TapeInfo a where
  -- Get Tape Id from (a + b_offset)
  getSumsList   :: Int -> [a] -> [a] -> [[a]]
  -- Get (b_offset, c_anti_offset) from (a + b_offset = c_offset)
  getOfsetsSums :: Int -> [a] -> [a] ->  [a]  ->  [(Int,Int)]

  -- Get length tape for big tapes (expl: base: 37, rank: 6)
  trapFinderLength :: HBase -> HRank -> [a] -> Int
  -- Get big offset between code
  trapFinderOffset :: [a] -> [a] -> Int

```

```haskell

showHCode $ (getPreset @HNum 10 5 1) *^> (hn 10 [1,2,3,4,5])

```

