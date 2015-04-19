--file project-euler/P14.hs
--Longest Collatz sequence under 1x10^6
import Data.List
import Data.Ord

main = do
    putStrLn $ show $ maxCollatz 999999

collatz :: Int -> Int -> Int
collatz n length
    | n == 1         = length
    | n `mod` 2 == 0 = collatz (n `div` 2) (length +1)
    | otherwise      = collatz (3*n + 1) (length +1)

maxCollatz limit = maybeAdd $ elemIndex (maximum xs) xs
    where xs = map (`collatz` 1) $ enumFromTo 1 limit

maybeAdd (Just x) = Just $ x + 1
