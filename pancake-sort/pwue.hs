import Control.Monad
import Data.Function (fix)
import Data.List (nub, sort, sortOn)

-- speichert zwischenergebnisse, damit nicht immer von vorne berechnet werden muss
memoizeL :: (Int -> Int -> a) -> [[a]]
memoizeL f = map (\x -> map (f x) [0..]) [0..]

memoize :: (Int -> Int -> a) -> Int -> Int -> a
memoize f n a = memoizeL f !! n !! a

-- bringt elemente des stapels in das interval [0..n-1] ohne die reihenfolge zu veraendern
normalize :: [Int] -> [Int]
normalize xs = map fst . sortOn snd . zip [0..] . map snd . sort $ zip xs [0 ..]

-- wendet die ersten n pfannkuchen
flipOp :: Int -> [Int] -> [Int]
flipOp n xs = normalize $ reverse (take (n - 1) xs) ++ drop n xs

-- legt einen pfannkuchen mit größe g auf den stapel, erhöht alle mit größe >= g um 1 und wendet dann die ersten n pfannkuchen
reverseFlipOp :: Int -> Int -> [Int] -> [Int]
reverseFlipOp n g xs =
    let pre = (g : map (\x -> if x >= g then x + 1 else x) xs)
     in reverse (take n pre) ++ drop n pre

flips :: Int -> [[Int] -> [Int]]
flips n = map flipOp [1 .. n]

reverseFlips :: Int -> [[Int] -> [Int]]
reverseFlips n = join [map (reverseFlipOp k) [0 .. n-1] | k <- [1 .. n]]

upperBound :: Int -> Int
upperBound n = ceiling (fromIntegral n / 1.5)

lowerBound :: Int -> Int
lowerBound n = floor (fromIntegral n / 3)

sortablePerms :: Int -> Int -> [[Int]]
sortablePerms = fix (memoize . recursiveFunc)

recursiveFunc :: (Int -> Int -> [[Int]]) -> Int -> Int -> [[Int]]
recursiveFunc k n 0 = [[0 .. n - 1]]
recursiveFunc k n a =
    nub
        [ rFlip s
        | rFlip <- reverseFlips n
        , s <- k (n - 1) (a - 1)
        , (a>1) || (rFlip s `notElem` k n 0)
        , all (\flip -> any (\b -> flip (rFlip s) `elem` k (n - 1) b) [(a-1) .. (upperBound n)]) (flips n)
        ]


kRec :: Int -> Int -> [[Int]]
kRec n 0 = [[0 .. n - 1]]
kRec n a = nub
        [ rFlip s
        | rFlip <- reverseFlips n
        , s <- kRec (n - 1) (a - 1)
        , all (\flip -> any (\b -> flip (rFlip s) `elem` kRec (n - 1) b) [(a-1) .. (upperBound n)]) (flips n)
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