import Data.List (sort, sortBy)
import AllNames (names)

compareLastNames :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
compareLastNames name1 name2 = if surname1 > surname2
                               then GT
                               else if surname1 < surname2
                                    then LT
                                    else EQ
  where surname1 = snd name1
        surname2 = snd name2

-- Using Guards for boolean expressions

-- compareLastNames :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
-- compareLastNames name1 name2
--   | surname1 > surname2 = GT
--   | surname1 < surname2 = LT
--   | otherwise = EQ
--   where
--       surname1 = snd name1
--       surname2 = snd name2