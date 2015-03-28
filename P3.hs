-- Largest prime factor of 600,851,475,143
-- Test to see if n is divisible evenly by a number in [2..n]
-- if yes then return that number and factorise on n`div`x
primeFacts n [] = n:[]
primeFacts n (x:xs) 
  | n `mod` x == 0 = x:primeFacts (n`div`x) [2..(n`div`x) -1]
  | otherwise = primeFacts n xs

main = do
  return $ maximum $ primeFacts 600851475143 [2..600851475143-1]
