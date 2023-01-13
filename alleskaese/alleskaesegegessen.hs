{-# LANGUAGE BangPatterns #-}

import Data.List (intercalate, sort)
import Data.Set qualified as Set
import SliceMultiSet
import System.CPUTime
import System.IO
import Text.Printf

newtype State = St (Cheese, [Slice], SliceMS, Int) deriving (Show)
instance Eq State where
    (St (_, su, sl, n)) == (St (_, su', sl', n')) = su == su' && sl == sl' && n == n'

instance Ord State where
    (St (_, su, sl, n)) `compare` (St (_, su', sl', n')) = (su, sl, n) `compare` (su', sl', n')

ordNub :: Ord a => [a] -> [a]
ordNub l = ordNub' l Set.empty
  where
    ordNub' [] _ = []
    ordNub' (x : xs) s
        | x `Set.member` s = ordNub' xs s
        | otherwise = x : ordNub' xs (Set.insert x s)

sortCheese :: Cheese -> Cheese
sortCheese (c1, c2, c3)
    | c1 <= c2 && c2 <= c3 = (c1, c2, c3)
    | c1 <= c3 && c3 <= c2 = (c1, c3, c2)
    | c2 <= c3 && c3 <= c1 = (c2, c3, c1)
    | c3 <= c1 && c1 <= c2 = (c3, c1, c2)
    | c2 <= c1 && c1 <= c3 = (c2, c1, c3)
    | c3 <= c2 && c2 <= c1 = (c3, c2, c1)

sortSlice :: Slice -> Slice
sortSlice (s1, s2)
    | s1 <= s2 = (s1, s2)
    | otherwise = (s2, s1)

nextStates :: State -> [State]
nextStates (St (cheese@(c1, c2, c3), su, sl, n)) =
    [ St (cheese', slice : su, sl', n)
    | slice <- compatible
    , let cheese' = addSlice cheese slice
    , let sl' = delete slice sl
    , fitsLater cheese' $ getMin sl'
    ]
        ++ [ St (cheese', slice : su, sl', n - 1)
           | slice <- [(c1, c2), (c1, c3), (c2, c3)]
           , let cheese' = addSlice cheese slice
           , let sl' = delete slice sl
           , fitsLater cheese' $ getMin sl'
           , n > 0
           , slice `notElem` compatible
           ]
  where
    compatible :: [Slice]
    compatible = findCompatible cheese sl
    fitsLater :: Cheese -> Slice -> Bool
    fitsLater (c1, c2, c3) (s1, s2) = (c1 <= s1) && (c2 <= s2)

sliceToState :: Int -> SliceMS -> Slice -> State
sliceToState n slices slice = St (sliceToCheese slice, [slice], delete slice slices, n)

sliceToCheese :: Slice -> Cheese
sliceToCheese (s1, s2) = sortCheese (1, s1, s2)

solve :: Int -> [Slice] -> [Slice]
solve n slices =
    (\(St (_, s, _, _)) -> reverse s)
        . head
        . (!! (length slices - 1))
        . iterate (ordNub . (>>= nextStates))
        $ [ sliceToState n slices' slice
          | slice <- reverse . sort . unique $ slices'
          ]
  where
    slices' = fromList slices

addSlice :: Cheese -> Slice -> Cheese
addSlice (c1, c2, c3) (s1, s2)
    | c1 == s1 && c2 == s2 = (c1, c2, c3 + 1)
    | c1 == s1 && c3 == s2 = sortCheese (c1, c2 + 1, c3)
    | c2 == s1 && c3 == s2 = sortCheese (c1 + 1, c2, c3)

slicesToCheeses :: [Slice] -> [Cheese]
slicesToCheeses (slice : slices) = scanl addSlice (sliceToCheese slice) slices

sliceFromString :: String -> Slice
sliceFromString s = (read s1, read s2)
  where
    ![s1, s2] = words s

-- reads slices from file at path which starts with number of slices
readSlices :: String -> IO [Slice]
readSlices path = do
    content <- readFile path
    let slices = map sliceFromString $ tail $ lines content
    return $ map sortSlice slices

main :: IO ()
main = do
    putStr "Pfad zur Datei: "
    hFlush stdout
    path <- getLine
    !slices <- readSlices path
    putStr "Anzahl der gegessene Käsestücke: "
    hFlush stdout
    n_str <- getLine
    let n = read n_str
    putStrLn "Starte Berechnung..."
    startTime <- getCPUTime
    let !order = solve n slices
    endTime <- getCPUTime
    let diff = (fromIntegral (endTime - startTime) :: Double) / (10 ^ 12)
    printf "Laufzeit: %.3f Sekunden\n" diff

    writeFile (path ++ "output.txt") $
        intercalate
            "\n"
            ( zipWith
                (\(s1, s2) (c1, c2, c3) -> printf "S: %d %d, K: %d %d %d" s1 s2 c1 c2 c3)
                order
                (slicesToCheeses order)
            )
    putStrLn $ "Fertig! Lösung in " ++ path ++ "output.txt gespeichert."
