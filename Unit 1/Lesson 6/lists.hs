myRepeat :: a -> [a]
myRepeat n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq start end aList = take end (drop start aList)

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf item aList = item `elem` firstHalf
    where firstHalf = take (length aList `div` 2) aList