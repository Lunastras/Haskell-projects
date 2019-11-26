module Q4 where

  normaliseSpace :: String -> String
  normaliseSpace [] = []
  normaliseSpace (x:[]) = [x]
  normaliseSpace (x:(s:xs)) = if(x == ' ' && s == ' ')
                                then normaliseSpace (s:xs)
                                else x:(normaliseSpace (s:xs))

  normaliseFront :: String -> String
  normaliseFront (' ':xs) = normaliseFront xs
  normaliseFront xs = xs

  normaliseBack :: String -> String
  normaliseBack xs =  reverse (normaliseFront (reverse xs))

  normalise :: String -> String
  normalise xs = normaliseSpace( normaliseFront (normaliseBack (xs)))

  prefix :: String -> String -> Bool
  prefix [] _ = True
  prefix _ [] = False
  prefix (s:ss) (x:xs) = if(s == x) then prefix ss xs
                          else False

  postfix :: String -> String -> Bool
  postfix a b = prefix (reverse a) (reverse b)

  substr :: String -> String -> Bool
  substr [] [] = True
  substr a [] = False
  substr a b = if (prefix a b) then True else substr a (tail b)

  substitute:: String -> String -> String -> String
  substitute _ _ [] = []
  substitute x y z = if (prefix x z) then (y ++ substitute x y (drop (length x) z)) else [head z] ++ substitute x y (tail z)
