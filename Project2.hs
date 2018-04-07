module Project2 (initialGuess, nextGuess, GameState) where
import Data.List

data GameState = GameState{
  guessTime:: Int
, repository:: [[String]]
} deriving (Show)



initialGuess :: Int -> ([String],GameState)
initialGuess size = (generator "BP" size, (GameState 1 []))

generator :: String -> Int -> [String] 
generator [] _ = []
generator _ 0 = []
generator str num = str: (generator str (num-1))


nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (prevGuess, prevGameState) feedback
    | (guessTime prevGameState) == 1  = secondGuess (prevGuess,prevGameState) feedback
    | otherwise = 
        (newGuess, GameState (guessTime prevGameState + 1) newRepository) where
            newRepository = filter (indicator prevGuess feedback) (repository prevGameState)
            newGuess   = longestElem newRepository

secondGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
secondGuess (prevGuess, prevGameState) (correct, kinds, colors) 
    = (newGuess, GameState (guessTime prevGameState + 1) newRepository) 
    where
        blackPieces = ["BK","BQ","BB","BB","BN","BN","BR","BR"]
        whitePieces = ["WK","WQ","WB","WB","WN","WN","WR","WR"]
        otherWhites = (length prevGuess)- correct- kinds- colors
        subseqOtherPieces = 
            [ x ++ y | 
                x <- (filter (\x -> length x == colors) (subsequences blackPieces)), 
                y <- (filter (\x -> length x <= otherWhites) (subsequences whitePieces))]
        appendCorrectPawns = (++) ((generator "BP" correct) ++ (generator "WP" kinds))
        newRepository = map appendCorrectPawns subseqOtherPieces
        newGuess = longestElem newRepository 

   
-- longestElem:: [[String]] -> [String]
-- longestElem = snd . maximum . map (\x -> (length x, x))

indicator :: [String] -> (Int,Int,Int) -> [String] -> Bool
indicator prevGuess feedback subseq
    | (examine prevGuess subseq) == feedback = True
    | otherwise = False

examine :: [String] -> [String] -> (Int,Int,Int)
examine prevGuess subseq = (correct, kinds, colors) where
    correctPieces = judge prevGuess subseq
    correct = length correctPieces
    colors = length (judge (map (!!0) prevGuess) (map (!!0) subseq)) - correct
    kinds = length (judge (map (!!1) prevGuess) (map (!!1) subseq)) - correct

judge :: Eq a => [a] -> [a] -> [a]
judge [] _ = []
judge (x:xs) subseqColorsOrKinds
  | x `elem` subseqColorsOrKinds = x : judge xs (delete x subseqColorsOrKinds)
  | otherwise = judge xs subseqColorsOrKinds
