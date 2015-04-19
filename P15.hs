--file project-euler/P15.hs
--number of ways to navigate a 20x20 grid with only down and right
import Data.List

main = do
    putStrLn $ show $ combinations 40 20

combinations initial changing = ((fac initial) `div` (fac changing)) `div` (fac remainder) where
    remainder = initial - changing

fac n = foldl' (*) 1 $ enumFromTo 1 n
