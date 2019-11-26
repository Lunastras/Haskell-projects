gibeBig :: IO ()
gibeBig =
   do putStr "Hello, world! Type a number bigger than 1000 \n"
      xs <- getLine
      if xs > "1000" then
         putStrLn "You got it! \n"
       else
         do putStrLn "Yeaa.... no \n"
            gibeBig

printStars :: Int -> IO()
printStars 0  = do putStr"\n"
printStars n  = do printStars1 (n)
                   printStars (n-1)

printStars1 :: Int -> IO()
printStars1 0 = do putStr"\n"
printStars1 x = do putStr "*"
                   printStars1 (x-1)


displayGame::[Int] -> IO()
displayGame [] = putStr ""
displayGame (x:xs) = do printStars1 x
                        displayGame xs

validMove :: [Int] -> Int -> Int -> Bool
validMove (x:xs) 1 b = if(x >= b) then True
                          else False
validMove (x:xs) a b = validMove xs (a-1) b
validMove [] a b = False
