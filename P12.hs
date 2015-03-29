-- first triangle number with more than 500 divisors
import qualified Data.List as L
main = do
  return $ interestingTN 500 10
interestingTN n seed
  | (length $ divisors tn) > n = tn 
  | otherwise = interestingTN n (seed+1)
    where tn = triangleNum seed
triangleNum n = L.foldl' (+) 0 $ enumFromTo 1 n

divisors n = n : 1 : (fillout $ filter ((==0).mod n) [2..limit])
  where limit = floor.sqrt.fromIntegral $ n
        fillout (x:[]) = x:[]
        fillout (x:xs) = x:(div n x):(fillout xs)
