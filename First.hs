generateList :: Int -> [[Int]]
generateList 1 = [[1], [2], [3]]
generateList n = (map (1:) (generateList (n-1))) ++ (map (2:) (generateList (n-1))) ++ (map (3:) (generateList (n-1)))


removeRedundant :: [[Int]] -> [[Int]]
removeRedundant []= []
removeRedundant (x:xs) | (lessFour x) = x : (removeRedundant xs)
                       | otherwise = removeRedundant xs

lessFour:: [Int] -> Bool
lessFour [] = True
lessFour (x:[]) = True
lessFour (x:y:xs) | x+y > 4 = False 
                     | otherwise = lessFour xs

main = do 
    let res = removeRedundant $ generateList 4
    putStrLn ( show res)