myRepeat :: a -> [a]
myRepeat n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq first last aList = take last (drop first aList)

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf item aList = item `elem` firstHalf
    where firstHalf = take (length aList `div` 2) aList