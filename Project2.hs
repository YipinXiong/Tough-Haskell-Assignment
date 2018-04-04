module Project2 (initialGuess, nextGuess, GameState) where
    --(repository . snd . initialGuess)
    data GameState = GameState{
      lastGuess:: [String]
    , repository:: [[String]]
    } deriving (Show)


    initialGuess :: Int -> ([String],GameState)
    initialGuess size = (generateBP size, GameState 1)
                        where generateBP num
                                | num == 0 = []
                                | otherwise = "BP": (generateBP (num-1))


    nextGuess:: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess (oldGuess,oldGS) (correct,kinds,colors)  
            | (correct,kinds,colors) == (0,0,0) = ([],GameState)
            | (repository oldGS)@(x:[]) (head . repository) $ oldGS, oldGS)
            | otherwise 
            where
                newRep = filter checkLastResult (fst GameState)
                newGuess = chooseLongest (newRep)

