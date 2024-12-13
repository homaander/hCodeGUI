module Code.HomaCode.Math (Math(..)) where

import Code.HomaCode.Data

class (Eq a, Show a) => Math a where
  (^+) :: a -> a -> a
  (^-) :: a -> a -> a
  (^*) :: a -> a -> a

  neg  :: a -> a

  -- default
  (^-) a b = a ^+ neg b


instance Math HNum where
  (^+) (HN a1 b1) (HN a2 b2) = if a1 == a2
                               then HN a1 $ (b1 + b2) `mod` a1
                               else error $ "Types: " <> show a1 <> ", " <> show a2
  (^*) (HN a1 b1) (HN a2 b2) = if a1 == a2
                               then HN a1 $ (b1 * b2) `mod` a1
                               else error $ "Types: " <> show a1 <> ", " <> show a2
  neg (HN a b) = HN a $ (a - b) `mod` a


instance Math a => Math [a] where
  (^+)= zipWith (^+)
  neg = map neg
  (^*) = zipWith (^*)


