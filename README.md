# hCodeGUI

***[Документация HCode](https://github.com/homaander/reference/blob/master/help/hcode/main.md)***

```haskell
import HomaCode

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

toHData @HNum 10 4 1234
```

