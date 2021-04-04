sumSquareOrSquareSum :: (Ord a, Num a) => a -> a -> a
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x * 2 + y * 2) ((x + y) * 2)

doubleDouble :: Num a => a -> a
doubleDouble x = (\dubs -> dubs * 2) (x * 2)

overwrite :: Num p1 => p2 -> p1
overwrite x = (\x ->
                (\x ->
                    (\x -> x) 4
                ) 3
              ) 2

inc :: Integer -> Integer
inc = (\x -> x + 1)

double :: Integer -> Integer
double = (\x -> x * 2)

square :: Integer -> Integer
square = (\x -> x ^ 2)

counter :: Num a => a -> a
counter x = (\x -> x + 1)
            ((\x -> x + 1)
             ((\x -> x) x))