import System.Environment (getArgs)

minmax [] = Nothing
minmax [x] = Just (x, x)
minmax (a:b:xs) = Just $ minmax' xs $ sort a b
    where minmax' [] lohi = lohi
          minmax' [x] lohi@(lo, hi)
            | x < lo = (x, hi)
            | x > hi = (lo, x)
            | otherwise = lohi
          minmax' (x:y:xs) (lo, hi) = minmax' xs $ newlo `seq` newhi `seq` (newlo, newhi)
              where (lo', hi') = sort x y
                    newlo = if lo' < lo then lo' else lo
                    newhi = if hi' > hi then hi' else hi

sort x y = if x < y then (x, y) else (y, x)

main = do
    args <- getArgs
    let [from, till] = map read args :: [Int]
    print $ minmax [from..till]
