--Smallest positive number divisible by [1..20]

allTrue :: [Bool] -> Bool
allTrue (x:xs)
  | x == False = False
  | otherwise = allTrue xs
allTrue [] = True

listModulus n (x:xs) = n `mod` x : listModulus n xs
listModulus _ [] = []


smallestDivis n = 
  case x of
       True  -> n
       False -> smallestDivis $ n + 20
  where x =allTrue $ map (==0) $ listModulus n [11..20]

main = do
  return $ smallestDivis 20
