fibSeq = 0:1: zipWith (+) fibSeq (tail fibSeq)
main = do
  return $ sum $ [x | x <- takeWhile (<4000000) fibSeq, even x]
