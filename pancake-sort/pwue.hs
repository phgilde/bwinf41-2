reverseFlip :: Int -> [Int] -> [Int]
reverseFlip x xs = reverse (take x xs) ++ drop x xs

addNext :: [Int] -> [Int]
addNext xs = (length xs + 1) : xs

nextHardest :: [Int] -> [Int]
nextHardest prev = undefined