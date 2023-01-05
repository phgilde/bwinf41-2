import Data.List (intercalate)
import Data.Set qualified as Set
import MultiSet
import System.CPUTime
import System.IO
import Text.Printf
import Control.Monad (msum)
import Data.Maybe (fromJust, isNothing)
import Control.Arrow ((&&&))
{-# LANGUAGE BangPatterns #-}

type Slice = (Int, Int)
type Cheese = (Int, Int, Int)

newtype State = St (Cheese, [Slice], MultiSet Slice) deriving (Show)

sortCheese :: Cheese -> Cheese
sortCheese (c1, c2, c3)
    | c1 >= c2 && c2 >= c3 = (c1, c2, c3)
    | c1 >= c3 && c3 >= c2 = (c1, c3, c2)
    | c2 >= c1 && c1 >= c3 = (c2, c1, c3)
    | c2 >= c3 && c3 >= c1 = (c2, c3, c1)
    | c3 >= c1 && c1 >= c2 = (c3, c1, c2)
    | c3 >= c2 && c2 >= c1 = (c3, c2, c1)

sortSlice :: Slice -> Slice
sortSlice (s1, s2)
    | s1 >= s2 = (s1, s2)
    | otherwise = (s2, s1)

removeSlice :: Cheese -> Slice -> Cheese
removeSlice (c1, c2, c3) (s1, s2)
    | c1 == s1 && c2 == s2 = (c1, c2, c3 - 1)
    | c1 == s1 && c3 == s2 = sortCheese (c1, c2 - 1, c3)
    | c2 == s1 && c3 == s2 = sortCheese (c1 - 1, c2, c3)
        
nextState :: State -> Maybe State
nextState (St (cheese@(c1, c2, c3), su, sl))
    | not . fitsLater cheese $ findMax sl = Nothing
    | (c1, c2) `member` sl = Just $ St (removeSlice cheese (c1, c2), (c1, c2) : su, delete (c1, c2) sl)
    | (c1, c3) `member` sl = Just $ St (removeSlice cheese (c1, c3), (c1, c3) : su, delete (c1, c3) sl)
    | (c2, c3) `member` sl = Just $ St (removeSlice cheese (c2, c3), (c2, c3) : su, delete (c2, c3) sl)
    | otherwise = Nothing

fitsLater :: Cheese -> Slice -> Bool
fitsLater (c1, c2, c3) (s1, s2) = (c1 >= s1) && (c2 >= s2)

maybeIter :: (a -> Maybe a) -> Int -> Maybe a -> Maybe a
maybeIter _ 0 m = m
maybeIter _ _ Nothing = Nothing
maybeIter f n m = maybeIter f (n - 1) (m >>= f)

volumen :: [Slice] -> Int
volumen = sum . map (uncurry (*))

fitsOutside :: Int -> Slice -> Bool
fitsOutside v (s1, s2) = v `rem` (s1 * s2) == 0

sliceToState :: Int -> MultiSet Slice -> Slice -> State
sliceToState v slices slice = St (sliceToCheese slice, [slice], delete slice slices)
  where
    sliceToCheese :: Slice -> Cheese
    sliceToCheese (s1, s2) = sortCheese ((v `div` (s1 * s2)) - 1, s1, s2)

solve :: [Slice] -> Maybe [Slice]
solve slices =
    (\(St (_, s, _)) -> Just s)
        =<< (msum
        . map (maybeIter nextState (length slices - 1))
        $ [ Just (sliceToState v slices' slice)
          | slice <- unique slices'
          , fitsOutside v slice
          ])
  where
    v = volumen slices
    slices' = fromList slices

slicesToCheeses :: [Slice] -> [Cheese]
slicesToCheeses (slice : slices) = scanl addSlice (sliceToCheese slice) slices
  where
    sliceToCheese :: Slice -> Cheese
    sliceToCheese (s1, s2) = sortCheese (s1, s2, 1)

    addSlice :: Cheese -> Slice -> Cheese
    addSlice (c1, c2, c3) (s1, s2)
        | c1 == s1 && c2 == s2 = sortCheese (c1, c2, c3 + 1)
        | c1 == s1 && c3 == s2 = sortCheese (c1, c2 + 1, c3)
        | c2 == s1 && c3 == s2 = (c1 + 1, c2, c3)

sliceFromString :: String -> Slice
sliceFromString s = (read s1, read s2)
  where
    [s1, s2] = words s

-- reads slices from file at path which starts with number of slices
readSlices :: String -> IO [Slice]
readSlices path = do
    content <- readFile path
    let slices = map sliceFromString $ tail $ lines content
    return $ map (uncurry max &&& uncurry min) slices

main :: IO ()
main = do
    putStr "Pfad zur Datei: "
    hFlush stdout
    path <- getLine
    slices <- readSlices path
    startTime <- getCPUTime
    let !order = solve slices
    endTime <- getCPUTime
    let diff = (fromIntegral (endTime - startTime) :: Double) / (10 ^ 12)
    printf "Laufzeit: %.3f Sekunden" diff
    hFlush stdout
    if isNothing order
        then putStrLn "Keine Lösung gefunden"
    else writeFile (path ++ "output.txt") $
        intercalate
            "\n"
            ( zipWith
                (\(s1, s2) (c1, c2, c3) -> printf "S: %d %d, K: %d %d %d" s1 s2 c1 c2 c3)
                (fromJust order)
                (slicesToCheeses $ fromJust order)
            )
