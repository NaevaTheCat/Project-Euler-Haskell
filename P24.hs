import qualified Data.List as L

main = do
    putStrLn.(!! 999999) . L.sort . L.permutations $ "0123456789"
