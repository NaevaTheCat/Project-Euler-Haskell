import qualified Data.Char as C
main = do
  bignumtext <- readFile "P8num.txt"
  let bignum = concat $ lines bignumtext
  return $ maximum . prods . mapRead . thirteens $ bignum

thirteens :: String -> [String]
thirteens xs@(x:xt) = 
  case length xs of
       12 -> []
       _ -> (map (xs !!) [0..12]) : thirteens xt
mapRead :: [String] -> [[Int]]
mapRead = map $ map C.digitToInt

prods :: [[Int]] -> [Int]
prods = map (foldr (*) 1)
