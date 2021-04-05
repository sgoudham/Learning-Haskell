import Data.List (sort, sortBy)
import AllNames (names)

compareSurname :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
compareSurname name1 name2 = if surname1 > surname2
                               then GT
                               else if surname1 < surname2
                                    then LT
                                    else EQ
      where surname1 = snd name1
            surname2 = snd name2

compareFullName :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareFullName name1 name2 = if surname1 > surname2
                               then GT
                               else if surname1 < surname2
                                    then LT
                                    else if forename1 > forename2
                                         then GT
                                         else if forename1 < forename2
                                              then LT
                                              else EQ
      where surname1 = snd name1
            surname2 = snd name2
            forename1 = fst name1
            forename2 = fst name2

{- Using Guards for boolean expressions

compareSurname :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
compareSurname name1 name2
  | surname1 > surname2 = GT
  | surname1 < surname2 = LT
  | otherwise = EQ
  where
      surname1 = snd name1
      surname2 = snd name2

compareFullName :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
compareFullName name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2 
-}