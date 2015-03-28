-- largest palidrome product of two three digit numbers.

isPalindrome x =  (== x) . read . reverse . show $ x

multiples = [x*y | x <- [100..999],y <- [100..999],isPalindrome (x*y)]

main = do
  return $ maximum multiples
