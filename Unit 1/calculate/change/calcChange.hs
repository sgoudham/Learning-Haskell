calcChange :: (Ord p, Num p) => p -> p -> p
calcChange owed given = if change > 0
                        then change
                        else 0
    where change = given - owed

inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = x * 2

square :: Num a => a -> a
square x = x * x

evenOrOdd :: Integral a => a -> a
evenOrOdd n = if even n
              then n - 2
              else 3 * n + 1
