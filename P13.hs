main = do
  n <- readFile "P13num.txt"
  let nL = lines n
  let nn = mapReadInt nL  
  putStrLn $ show $ sum nn

mapReadInt :: [String] -> [Integer]
mapReadInt x = map read x
