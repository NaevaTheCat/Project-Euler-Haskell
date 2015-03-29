-- sum of all primes less than 2,000,000
main = do
  return $ sumBelow 2000000 primeList

sumBelow x (p:ps)
  | p < x = p + sumBelow x ps
  | otherwise = 0
primeList = [2,3,5] ++ (diffLists [7,9..] notPrimeList)
notPrimeList = foldr1 huh $ map primeMults $ tail primeList
huh (x:xt) ys = x : (mergeLists xt ys)
primeMults :: Int -> [Int]
primeMults prime = [prime * n | n <-[prime,prime+2..]]

-- merge two infinite lists into one ordered list without
-- duplicates
mergeLists xs@(x:xt) ys@(y:yt) =
  case compare x y of
       LT -> x : mergeLists xt ys
       EQ -> x : mergeLists xt yt
       GT -> y : mergeLists xs yt
-- form an infinite list from the set difference of 2 lists
diffLists xs@(x:xt) ys@(y:yt) =
  case compare x y of
       LT -> x: diffLists xt ys
       EQ -> diffLists xt yt
       GT -> diffLists xs yt
