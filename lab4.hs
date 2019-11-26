rev1::[a] -> [a]
rev1 [] = []
rev1 (x:xs) = rev1 xs ++ [x]

rev2::[a] -> [a]
rev2 [] = []
rev2 x = last x : rev2 (init x)

rev3::[a] -> [a]
rev3 [] = []
rev3 xs = revaux xs []

revaux::[a] -> [a] -> [a]
revaux [] acc = acc
revaux (x:xs) acc = revaux xs ([x] ++ acc)



extractDigits:: String -> String
extractDigits [] = []
extractDigits (x:xs) = if (x >= '0' && x <= '9')
                     then [x] ++ extractDigits xs
                     else extractDigits xs


insertElement:: Ord a => a -> [a] -> [a]
insertElement x [] = [x]
insertElement x (y:ys) = if (x < y)
                            then x:(y:ys)
                            else [y] ++ insertElement x ys

insertionSort:: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertElement x (insertionSort(xs))


palindrome:: Eq a => [a] -> Bool
palindrome [] = True
palindrome x = x == rev1 x

square:: Num a => a -> a
square a = a * a

test0:: Num a => [a] -> [a]
test0 x = [a*4| a <- x]

test1:: Num a => [a] -> [a]
test1 x = [square a | a <- x]

test2:: [String] -> [String]
test2 [] = []
test2 x = [a| a <- x, palindrome a]

test3:: [String] -> [Bool]
test3 [] = []
test3 x = (==True ) filter [palindrome a| a <- x]
