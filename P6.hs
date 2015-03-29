-- Difference between (sum x)^2 and sum(x^2) x from N

sumOfSquares = sum $ map (^2) [1..100]
squaredSum = (^2) $ sum [1..100]
main = do
  return $ squaredSum - sumOfSquares
