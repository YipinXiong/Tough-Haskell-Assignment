module Project2 (initialGuess, nextGuess, GameState) where
    --(repository . snd . initialGuess)
    data GameState = GameState{
      lastGuess:: [String]
    , repository:: [[String]]
    } deriving (Show)

    initialGuess :: Int -> ([String],GameState)
    initialGuess size = case size of 
         1  -> (["BB"], GameState ["BB"] $ generateGS 1)
         2  -> (["BB","BK"], GameState ["BB","BK"] $ generateGS 2)
         3  -> (["BB","BK","BN"], GameState ["BB","BK","BN"] $ generateGS 3)
         4  -> (["BB","BK","BN","BP"], GameState ["BB","BK","BN","BP"] $ generateGS 4)
         5  -> (["BB","BK","BN","BP","BQ"], GameState ["BB","BK","BN","BP","BQ"] $ generateGS 5)
         _  -> (["BB","BK","BN","BP","BQ","BR"], GameState ["BB","BK","BN","BP","BQ","BR"] $ generateGS 6)
        where generateGS num = case num of  
                1 -> pieces
                2 -> (repository . snd . initialGuess)  1 ++ [x++y | x <- pieces, y <- pieces]
                3 -> (repository . snd . initialGuess)  2 ++ [x++y++z | x <- pieces, y <- pieces, z <- pieces]
                4 -> (repository . snd . initialGuess)  3 ++ [x++y++z++a | x <- pieces, y <- pieces, z <- pieces, a <- pieces]
                5 -> (repository . snd . initialGuess)  4 ++ [x++y++z++a++b | x <- pieces, y <- pieces, z <- pieces, a <- pieces, b<- pieces]
                _ -> (repository . snd . initialGuess)  5 ++ [x++y++z++a++b++c | x <- pieces, y <- pieces, z <- pieces, a <- pieces, b<- pieces, c<-pieces]
                where 
                    pieces = [[x++y] | x <-["B","W"], y <-["B","K","N","P","Q","R"]]

    nextGuess:: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess (oldGuess,oldGS) (correct,kinds,colors)  
            | (repository oldGS)@(x:[]) (head . repository) $ oldGS, oldGS)
            | otherwise 
            where
                newRep = filter checkLastResult (fst GameState)
                newGuess = chooseLongest (newRep)

