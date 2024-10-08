-- Solves https://arkusze.pl/maturalne/informatyka-2014-grudzien-probna-rozszerzona-2.pdf Zadanie 4.1 - 4.3 (Without graphs)
-- Outputs to answers.txt

module Main where

formatData :: [(Double, Double, Double)] -> String
formatData dataPoints = unlines $ map (\(t, x, y) -> unwords [show t, show x, show y]) dataPoints

formatDataDist :: [Double] -> String
formatDataDist = unlines . map show
    
f :: (Double, Double, Double) -> (Double -> Double) -> Double
f (radius, time, period) trig_function = radius * (trig_function (2 * pi * time / period))

fx, fy :: (Double, Double, Double) -> Double
fx args = f args sin
fy args = f args cos

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- 4.1 | 4.2
trace_point :: (Double, Double, Double, Double, Double) -> (Double -> Double -> Double -> Bool) -> [(Double, Double, Double)]
trace_point (radius, period, dt, time, speed) condition
    | condition time x y = []
    | otherwise = (time, x, y) : trace_point (radius + (speed * dt), period, dt, time + dt, speed) condition
    where
        x = fx (radius, time, period)
        y = fy (radius, time, period)

-- 4.3
accum_dist :: [(Double, Double, Double)] -> Double -> Double -> Double
accum_dist [] _ _ = 0
accum_dist ((_, nx, ny) : ndata) x y = dist (x, y) (nx, ny) + accum_dist ndata nx ny

map_dist :: [(Double, Double, Double)] -> Double -> Double -> [Double]
map_dist [] _ _ = []
map_dist ((t, nx, ny) : ndata) x y = dist (x, y) (nx, ny) : (map_dist ndata nx ny)


main :: IO ()
main = do
    let results41 = trace_point (5.0, 12.5, 0.05, 3, 0) (\_ x y -> y > x)
        results42 = trace_point (0, 10.0, 0.5, 0, 1) (\time _ _ -> time > 10)
        lastT = last (map (\(t, _, _) -> t) results41)
        distances = map_dist results42 0 0
        totalAccumDist = accum_dist results42 0 0

    writeFile "answers.txt" $ 
        "4.1\ntime x y\n" ++ formatData results41 ++
        "\n\nTime: " ++ show lastT ++
        "\n\n4.2\ntime x y\n" ++ formatData results42 ++
        "\n\n4.3\ndistances\n" ++ formatDataDist distances ++
        "\n\nTotal Distance: " ++ show totalAccumDist
