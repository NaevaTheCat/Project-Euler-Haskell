import qualified Data.List as L
main = do
    let l = map (seqLength []) $ map modSeq [2..999]
    let maxL = maximum l
    putStrLn . show . (+2) . length . takeWhile (< maxL) $ l
modSeq :: Integral a => a -> [a]
modSeq d= L.unfoldr modfunc (1,mod 10 d) where
    modfunc = \(n,r) -> Just (n,(r,mod (r*10) d))

seqLength xs (y:ys) 
    | y == 0 = 0
    | otherwise =
        case (y `elem` xs) of
            False -> seqLength (y:xs) ys
            True  -> length . dropWhile (/=y) . reverse $ xs

