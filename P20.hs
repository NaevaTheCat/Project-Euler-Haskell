--file project-euler/P16.hs
import Data.Char
main = do
    putStrLn $ show $ sumDigits (fac 100)

fac n = foldl (*) 1 $ enumFromTo 1 n
sumDigits n = sum $ map digitToInt $ show n
