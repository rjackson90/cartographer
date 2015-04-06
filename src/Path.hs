module Path
( Point(..)
, plot
) where

type Point = (Int, Int)

quadratic_bezier :: Float -> Int -> Int -> Int -> Int
quadratic_bezier t start control end = round $ term1 + term2 + term3
    where
        t2 = t * t
        mt = 1.0 - t 
        mt2 = mt * mt
        term1 = (fromIntegral start) * mt2
        term2 = (fromIntegral control) * 2.0 * mt * t
        term3 = (fromIntegral end) * t2

plot :: Point -> Point -> Point -> Int -> [Point]
plot start control end samples = 
    [(
        (quadratic_bezier t (fst start) (fst control) (fst end))
    ,   (quadratic_bezier t (snd start) (snd control) (snd end)) )
    | t <- ts]
    where   increment = 1.0 / (fromIntegral samples)
            ts = map (\a -> (fromIntegral a) * increment) [0, 1..samples] :: [Float]
