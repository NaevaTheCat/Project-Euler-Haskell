import qualified Data.List as L
main = do
  numgridtext <- readFile "P11num.txt"
  let numgrid = mapRead . map words . lines $ numgridtext
  return $ maximum $ allLists numgrid

allLists ls = (diagLists1 ls) ++ (diagLists2 ls) ++ (diagLists3 ls) ++ (diagLists4 ls) ++ (downLists ls) ++ (acrossLists ls)
mapRead :: [[String]] -> [[Int]]
mapRead = map $ map read

dropper _ [] = []
dropper n ls = drop n (head ls) : dropper (n+1) (tail ls)

flipper ls = map reverse ls

diagLists1 ls = acrossLists $ L.transpose $ dropper 0 ls
diagLists2 ls = diagLists1 $ L.transpose ls
diagLists3 ls = diagLists1 $ flipper ls
diagLists4 ls = diagLists1 $ L.transpose $ flipper ls
downLists :: [[Int]] -> [Int]
downLists ls = acrossLists $ L.transpose ls

acrossLists :: [[Int]] -> [Int]
acrossLists ls@(lh:[]) = prodsOfFours (head ls)
acrossLists ls = prodsOfFours (head ls) ++  (acrossLists $ tail ls)

prodsOfFours xs@(x:xt) 
  | length xs <4 = []
  | otherwise = (foldl (*) 1 $ take 4 xs) : prodsOfFours xt

