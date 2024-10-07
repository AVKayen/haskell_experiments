module Main where

-- / Change below to modify the behaviour of the program

minValue :: Double
minValue = 0

maxValue :: Double
maxValue = pi / 2

epsilon :: Double
epsilon = 0.000001

f :: Double -> Double
f s = sin s     -- sin(x)

g :: Double -> Double
g s = s ** 2    -- x^2

-- \

fc :: Double -> Double
fc s = f s - g s

bisect :: (Double, Double) -> Double
bisect (a, b)
    | abs (fc c) < epsilon = c          -- return C if it's close enough to 0
    | (fc c * fc a) < 0 = bisect (a, c) -- further bisect at range from a to (a+b)/2
    | otherwise = bisect (c, b)         -- ditto, range from (a+b)/2 to b
    where c = (a+b)/2

main :: IO ()
main = putStrLn (show (bisect (minValue, maxValue)))