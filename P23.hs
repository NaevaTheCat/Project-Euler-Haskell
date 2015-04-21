--file project-euler/P23.hs
--The sum off all n that cannot be written as
--the sum of 2 abundant numbers with n from N+

--given a number k, look at the next abundant
--number 12 less n. At that number look from 12
--to sum n m > k. If fail move down 1 abundant
--number and exclude all previous ones from search
--except m last. If hit n=m stop and return that
--number. Limit is 28123

--23-1 included by default
--[28123 - 24] to test
--can cull by abundants x 2 to make shorter
import qualified Data.List as L
--abundants and reversed abundants
main = do
    putStrLn . show . ((sum [1..23]) +) . sum $ unSums
unSums :: [Int]
unSums = filter (\x -> fromTop x reversedAs abundants) culledlist

fromTop :: Int -> [Int] -> [Int] -> Bool
fromTop x ras@(ra:rat) as@(a:at)
    | ra <= x+12 = fromBottom x ras as
    | ra >  x+12 = fromTop x rat as

fromBottom x ns@(n:nt) as@(a:at)
    | n+a == x = False
    | n == a   = True
    | n+a < x  = fromBottom x ns at
    | n+a > x  = fromTop x nt as

culledlist = ([24..28123] L.\\) . map (*2) $ abundants
reversedAs :: [Int]
reversedAs = reverse abundants

abundants :: [Int]
abundants = map fst $ filter (\(a,b) -> b > a) $ 
    map (\x -> (x,sum.divisors $ x)) [12..28111]
--largest possible useful abundant number 28111
divisors n = 1 : (fillout $ filter ((==0).mod n) [2..limit])
  where limit = floor.sqrt.fromIntegral $ n
        fillout []     = [] --fix for tiny numbers
        fillout (x:[]) 
            | (div n x) == x = x:[]
            | otherwise      = x:(div n x):[]
        fillout (x:xs) = x:(div n x):(fillout xs)
