--just need to compare the left most and right most path

main = do
    leftT <- readFile "p067_triangle.txt"
    let left = reverse $ mapReadInt $ map words . lines $ leftT
    putStrLn $ show $ bestPath left

bestPath triangle = head $ foldl collapse (head triangle) (tail triangle)

collapse lower higher = zipWith (+) higher best
    where best = zipWith max lower shifted
          shifted = tail lower

mapReadInt x = map (map read) x
