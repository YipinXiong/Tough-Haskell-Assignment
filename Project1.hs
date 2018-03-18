module Project1 (elementPosition, everyNth, elementBefore) where

    elementPosition :: (Eq t) => t -> [t] -> Int
    elementPosition elt (x:xs)
        | (elt `elem` (x:xs)) == False = 0 
        | elt == x = 1
        | otherwise = 1 + elementPosition elt xs

    everyNth :: Int -> [t] -> [t]
    everyNth n lst 
        | n == 0 = error "n cannot be zero"
        | (length lst) < n = []
        | otherwise = (lst !! (n-1)) : (everyNth n (drop n lst))


    elementBefore :: (Eq a) => a -> [a] -> Maybe a
    elementBefore n lst 
        | elmtPosition > 1 && lst /= [] = Just (last partBeforeN)
        | otherwise = Nothing
        where 
              pos elmt (x:xs)
                | (elmt `elem` (x:xs)) == False = 0 
                | elmt == x = 1
                | otherwise = 1 + pos elmt xs 
              elmtPosition = pos n lst
              partBeforeN = take (elmtPosition-1) lst