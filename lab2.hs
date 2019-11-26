coolNice :: [Char] -> [Char] -> Bool
coolNice = \x -> \y -> x == y

swap :: (x,y) -> (y,x)
swap (x,y) = (y,x)

middle :: (x,y,z) -> y
middle (x,y,z) = y

pallinedrome :: Eq xs => [xs] -> Bool
pallinedrome xs = xs == reverse xs

add :: Num x => x -> x -> x -> x
add x y z = x + y + z

ordered :: Ord a => (a,a,a) -> Bool
ordered (x,y,z) = x<=y && y<=z
