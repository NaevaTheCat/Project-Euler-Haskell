--file project-euler/P12.hs
--Sum of all amicable numbers < 1000
--a and b are amicable if \sigma d(a)=b
--sigma d(b)=a
import qualified Data.List as L
-- a and sum d(a)
main = do
    putStrLn . show . sum . concat . amicable $ nAndSumD
nAndSumD :: [(Int,Int)]
nAndSumD = zip [1..9999] $ map (sum . divisors) [1..9999] 

amicable :: [(Int,Int)] -> [[Int]]
amicable [] = []
amicable ((a,sA):bs) = 
    let result = map fst . filter ((a ==).snd).filter ((sA ==).fst) $ bs
        in case result of
                [] -> amicable bs
                _  -> [a] : result : amicable bs
-- Need to find all fst a = snd b && fst b = snd a
divisors n = 1 : (fillout $ filter ((==0).mod n) [2..limit])
  where limit = floor.sqrt.fromIntegral $ n
        fillout []     = [] --fix for tiny numbers
        fillout (x:[]) 
            | (div n x) == x = x:[]
            | otherwise      = x:(div n x):[]
        fillout (x:xs) = x:(div n x):(fillout xs)
