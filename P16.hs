--file project-euler/P16.hs
import Data.Char
main = do
    putStrLn $ show $ sumDigits (2^1000)

sumDigits n = sum $ map digitToInt $ show n
