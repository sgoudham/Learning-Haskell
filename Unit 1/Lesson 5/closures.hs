inc :: Num a => a -> a
inc n = n + 1
double :: Num a => a -> a
double n = n * 2
square :: Num a => a -> a
square n = n ^ 2

isEven :: Integral p => (p -> p) -> p -> p
isEven f x = if even x
             then f x
             else x

ifEvenInc :: Integer -> Integer
ifEvenInc = isEven inc
ifEvenDouble :: Integer -> Integer
ifEvenDouble = isEven double
ifEvenSquare :: Integer -> Integer
ifEvenSquare = isEven square

-- Generating URL's for an API

getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource id = host ++ "/" ++
                                        resource ++ "/" ++
                                        id ++ "?token=" ++
                                        apiKey

genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host = (\apikey resource id -> getRequestURL host apikey resource id)
genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
genResourceRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genResourceRequestBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = genHostRequestBuilder "http://example.com"
myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"
myExampleResourceUrlBuilder :: [Char] -> [Char]
myExampleResourceUrlBuilder = genResourceRequestBuilder exampleUrlBuilder "1337hAsk3ll" "books"

-- Using Partial application

exampleUrlBuilderPA :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilderPA = getRequestURL "http://example.com"
myExampleUrlBuilderPA :: [Char] -> [Char] -> [Char]
myExampleUrlBuilderPA = exampleUrlBuilderPA "1337hAsk3ll"
myExampleResourceUrlBuilderPA :: [Char] -> [Char]
myExampleResourceUrlBuilderPA = myExampleUrlBuilderPA "books"

subtract2 :: Integer -> Integer
subtract2 = flip (-) 2
-- subtract2 x = flip (-) 2 x

binaryPartialApplication :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
binaryPartialApplication binaryFunc arg = (\x -> binaryFunc arg x)
takeFrom4 :: Integer -> Integer
takeFrom4 = binaryPartialApplication (-) 4