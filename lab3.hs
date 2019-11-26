safetail :: (Eq a) => [a] -> [a]
safetail xs = if(xs == []) then []
              else tail xs

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 ::  [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs



firstNSquares :: Int -> [Int]
firstNSquares n = [x^2| x <- [1..n]]

squaresSmallerThanN :: Int -> [Int]
squaresSmallerThanN n = [x| x <- [1..n] , x^2 < n]

list3 :: [Int]
list3 = [x| x <- [100..300], (x `mod` 11) /= 0]

palindrome :: Int -> Bool
palindrome xs = show xs == reverse (show xs)

list4 :: [Int]
list4 = [x| x <- [100..300], palindrome x]

list5:: [Int]
list5 = [x| x <- [100..300], palindrome x, (x `mod` 11) /= 0]



sumOfDig :: Int -> Int
sumOfDig 0 = 0
sumOfDig a = (a `mod` 10) + sumOfDig (a `div` 10)

perfect :: Int -> Bool
perfect a = a == sum [x| x <- [1..(a-1)], a `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x| x <- [1..n], perfect x]
