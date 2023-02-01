import Control.Monad
import Data.Function (fix)
import Data.List (nub, sort, sortOn)
import Data.MemoCombinators (memo2, integral)
import qualified Data.Set as Set

ordNub :: Ord a => [a] -> [a]
ordNub l = ordNub' l Set.empty
  where
    ordNub' [] _ = []
    ordNub' (x : xs) s
        | x `Set.member` s = ordNub' xs s
        | otherwise = x : ordNub' xs (Set.insert x s)


-- bringt elemente des stapels in das interval [0..n-1] ohne die reihenfolge zu veraendern
normalize :: [Int] -> [Int]
normalize xs = map fst . sortOn snd . zip [0 ..] . map snd . sort $ zip xs [0 ..]

-- wendet die ersten n pfannkuchen
flipOp :: Int -> [Int] -> [Int]
flipOp n xs = normalize $ reverse (take (n - 1) xs) ++ drop n xs

-- legt einen pfannkuchen mit größe g auf den stapel, erhöht alle mit größe >= g um 1 und wendet dann die ersten n pfannkuchen
reverseFlipOp :: Int -> Int -> [Int] -> [Int]
reverseFlipOp n g xs =
    let pre = (g : map (\x -> if x >= g then x + 1 else x) xs)
     in normalize $ reverse (take n pre) ++ drop n pre

flips :: Int -> [[Int] -> [Int]]
flips n = map flipOp [1 .. n]

reverseFlips :: Int -> [[Int] -> [Int]]
reverseFlips n = [reverseFlipOp k g | g <- [0 .. n], k <- [1 .. n]]

upperBound :: Int -> Int
upperBound n = ceiling (fromIntegral n / 1.5)

lowerBound :: Int -> Int
lowerBound n = floor (fromIntegral n / 3)

kPredicate :: ([Int] -> [Int]) -> Int -> Int -> [Int] -> Bool
kPredicate rFlip n a xs = and
    [ or
        [ flip (rFlip xs) `elem` kRec (n - 1) b
        | b <- [(a - 1) .. (upperBound n)]
        ]
    | flip <- flips n
    ]

-- rekursive funktion, die die permutationen berechnet
kRec :: Int -> Int -> [[Int]]
kRec = memo2 integral integral kRec'
    where
    kRec' n 0 = [[0 .. n - 1]]
    kRec' n a = ordNub
        [ rFlip s
        | rFlip <- reverseFlips n
        , s <- kRec (n - 1) (a - 1)
        , a > 1 || (rFlip s `notElem` kRec n 0)
        , kPredicate rFlip n a s
        ]

-- n und a werden eingegeben und an die rekursive funktion übergeben
main :: IO ()
main = do
    putStrLn "n:"
    n <- readLn :: IO Int
    putStrLn "a:"
    a <- readLn :: IO Int
    let perms = kRec n a
    print $ head perms