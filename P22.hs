import qualified Data.List as L
main = do
    rawnames <- readFile "P22names.txt"
    let filteredNames = filter (/='"') rawnames
    let sortedNames = L.sort. wordsWhen (==',') $ filteredNames
    putStrLn $ show $ snd $ L.foldl' scoreName (1,0) sortedNames

scoreName :: (Int,Int) -> String -> (Int,Int)
scoreName (p,lastval) name = (p+1,val) where
                            val = (+lastval).(* p) . sum . map lookUp $ name

lookUp :: Char -> Int
lookUp l = let (Just e) = lookup l alphToNum in e
alphToNum = zip ['A'..'Z'] [1..]

wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen _ [] = []
wordsWhen p xs = let (first,last) = break p xs 
                     rest = drop 1 last
                     in first : wordsWhen p rest
