data GameState = GameState{
      size :: Int
      ,leftRole :: String
      ,possibleRole :: [String] 
    } deriving (Show)

initialGuess:: Int -> ([String],GameState)
initialGuess size 
	| size == 0 = ([], GameState)
	
	| size > 6 = (generateBP size, GameState size "BRNKQ" []) 
                    where generateBP num
                            | num == 0 = []
                            | otherwise = "BP": (generateBP (num-1))

-- nextGuess:: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
-- nextGuess (lastGuess, gameState) (correct, kinds, colors) = 
--     (generater, GameState length leftrole postiverole)
--     where


