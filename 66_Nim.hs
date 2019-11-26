import Text.Printf
import Data.Complex

--Question 1

max1::Int->Int->Int->Int
max1 a b c = if (a > b && a > c) then a
              else if(b > c && b > a) then b
                else c

max2::Int->Int->Int->Int
max2 a b c = myMax (myMax a b) c

myMax::Int-> Int->Int
myMax a b = if((a - b > 0 )) then a
            else b

checkCorrectness :: Int -> Int -> Int -> Bool
checkCorrectness x y z = ((max1 x y z) == (max2 x y z))

--Question 2

--decimalFormat :: Float -> String
--decimalFormat a = (printf "%.2f" a)

--bambini is more expensive than Famiglia (5.82 and 2.02)
--luigi :: Float -> Float -> Float
--luigi a b = (read (decimalFormat ((a * 0.002 + b * 0.6) * 1.6)) :: Float)

myRound :: Float -> Float
myRound x = (fromInteger $ round $ x * (10^2)) / (10.0^^2)

 --Bambini (6.33) is more expensive than Famiglia (4.49)
 --The first value is the number of topping, while the second one is the diameter.
luigi :: Float -> Float -> Float
luigi x y = do let a = (x * 0.6 + pi * ((y/2)**2) * 0.002) * 1.6
               myRound a

--Question 3

--recursive
countDigits1:: String -> Int -> Int
countDigits1 [] a = a
countDigits1 (s:xs) a = if (s >= '0' && s <= '9')
                          then countDigits1 xs (a+1)
                          else countDigits1 xs a

--List comprehension
countDigits2:: String -> Int
countDigits2 xs = sum [ (if (s >= '0' && s <= '9')
                          then 1
                          else 0) | s <- xs ]

--Higher order function
countDigits3:: String -> Int
countDigits3 s = countDigits1 s 0


--Question 5

--Call this method to play Nim with 2 players,
--The argument is the piles of coins used for the game
--(E.g. playNim [3,4,1])
playNim:: [Int] -> IO()
playNim xs = do putStrLn "Hello! Please write the name for player 1."
                input1 <- getLine
                putStrLn "Please write the name for player 2."
                input2 <- getLine
                gameOfNim xs True (input1, input2)

--The game of nim in and of itself.
gameOfNim :: [Int] -> Bool -> (String, String) -> IO ()
gameOfNim xs isPlayer1  (p1, p2)= if(won xs)
                   then gameEnd (if isPlayer1 then p2 else p1)
                   else do displayGame xs 1
                           if (isPlayer1) then putStr ( p1 ++ ", make your move! \n")
                            else putStr (p2 ++ ", make your move! \n")
                           (x, y) <- getMove2 xs
                           let newxs = takeAway xs x y
                           gameOfNim newxs (not isPlayer1) (p1, p2)

-- Checks if the game is finished
won::[Int] -> Bool
won xs = all (==True) [x == 0| x <- xs]

validMove :: [Int] -> Int -> Int -> Bool
validMove (x:xs) 1 b = if(x >= b) then True
                      else False
validMove (x:xs) a b = validMove xs (a-1) b
validMove [] a b = False

takeAway:: [Int] ->  Int -> Int -> [Int]
takeAway xs a b = (take (a) xs) ++ [((head (drop (a) xs)) - b)] ++ (drop (a + 1) xs)

isNumber:: String -> Bool
isNumber [] = False
isNumber xs = all (==True) [(x <= '9' && x >= '0')| x <- xs]

getMove1 :: [Int] -> IO(Int,Int)
getMove1 xs = do putStrLn("Select from which pile?")
                 input1 <- getLine
                 putStrLn("How many coins?")
                 input2 <- getLine
                 if ((isNumber input1) && (isNumber input2))
                    then do let a = (read input1 :: Int)
                            let y = (read input2 :: Int)
                            return (a,y)
                    else do putStr "Invalid Input! Type again! \n"
                            getMove2 xs

getMove2 :: [Int] -> IO(Int,Int)
getMove2 xs = do (x, y) <- getMove1 xs
                 if (validMove xs x y)
                   then return (x - 1, y)
                     else do
                        putStrLn("This is not a valid move!")
                        putStr("Please enter another input: \n")
                        getMove2 xs


printStars:: Int -> IO ()
printStars 0 = do putStr"\n"
printStars x = do putStr "*"
                  printStars (x-1)


displayGame::[Int] -> Int -> IO ()
displayGame [] _ = putStr ""
displayGame (x:xs) n = do putStr (show n)
                          putStr ") "
                          printStars x
                          displayGame xs (n+1)

gameEnd:: String -> IO ()
gameEnd player = putStrLn (player ++ " wins!")

--Question 6

gameOfNimAI :: ([Int] -> (Int,Int)) -> [Int] -> Bool -> String ->  IO ()
gameOfNimAI f xs playerTurn playerName = if(won xs)
                                    then gameEnd (if playerTurn then "The AI" else playerName)
                                    else do displayGame xs 1
                                            if playerTurn
                                              then do (x, y) <- getMove2 xs
                                                      let newxs = takeAway xs x y
                                                      gameOfNimAI f newxs (not playerTurn) playerName
                                              else do putStrLn "AI's turn!"
                                                      let (x, y) = f xs
                                                      let ys = takeAway xs (if (validMove xs (x+1) y) then x else (x-1)) y
                                                      putStrLn ("The AI has taken " ++ (show y) ++ " coins from pile " ++ (show (x + 1)))
                                                      gameOfNimAI f ys (not playerTurn) playerName

--The game of Nim against and AI in and of itself.
playNimAI:: ([Int] -> (Int,Int)) -> IO()
playNimAI f = do putStrLn "Hello! Please tell me your name :)"
                 name <- getLine
                 putStrLn "Give me the piles!"
                 input1 <- getLine
                 let xs = (read input1 :: [Int])
                 gameOfNimAI f xs True name

-- Basic strategy for the AI. Takes the first non-empty piles
stratB:: [Int] -> (Int, Int)
stratB xs = do let ys = zip [1..] xs
               let (x,y) = head [(x, y)| (x, y) <- ys, y /= 0]
               (x - 1, y)

-- This is the improved strategy.
-- The AI will take the first non-empty pile
-- until there remain only two piles.
-- If the piles are not equal, it will make the bigger pile
-- one smaller than the other.
-- If they are equal, it will take one coin out of the first one.

-- First round of interaction:
-- *Main> nimAI stratI
-- Give me the piles!
-- [2,4,5]
-- **
-- ****
-- *****
-- Your turn!
-- 3
-- 1
-- **
-- ****
-- ****
-- AI's turn!
--
-- ****
-- ****
-- Your turn!
stratI:: [Int] -> (Int, Int)
stratI xs = do let ys = zip [1..] xs
               let ks = [(x, y)| (x, y) <- ys, y /= 0]
               if (length ks /= 2)
                  then do let (x, y) = head ks
                          (x - 1, y)
                  else do let z = head(tail xs)
                          let k = head xs
                          let (x, y) = head ks
                          let (a, b) = head(tail ks)
                          if k == z
                            then (x - 1, 1)
                            else if k > z
                                    then (x - 1, k-z)
                                    else (a - 1, z-k)
