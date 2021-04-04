inc :: Num a => a -> a
inc n = n + 1
double :: Num a => a -> a
double n = n * 2
square :: Num a => a -> a
square n = n ^ 2

isEven :: Integral p => (p -> p) -> p -> p
isEven myFunction x = if even x
                      then myFunction x
                      else x

ifEvenDouble :: Integral p => p -> p
ifEvenDouble x = isEven double x
ifEvenInc :: Integral p => p -> p
ifEvenInc x = isEven inc x
ifEvenSquare :: Integral p => p -> p
ifEvenSquare x = isEven square x
ifEvenCube :: Integral a => a -> a
ifEvenCube x = isEven (\x -> x ^ 3) x