{-# LANGUAGE BangPatterns #-}

import Data.List (intercalate, sort)
import Data.Set qualified as Set
import SliceMultiSet
import System.CPUTime
import System.IO
import Text.Printf

newtype State = St (Cheese, [Slice], SliceMS) deriving (Show)
instance Eq State where
    (St (_, su, sl)) == (St (_, su', sl')) = su == su' && sl == sl'

instance Ord State where
    (St (_, su, sl)) `compare` (St (_, su', sl')) = (su, sl) `compare` (su', sl')

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
nextStates (St (cheese, su, sl)) =
    [ St (cheese', slice : su, sl')
    | slice <- findCompatible cheese sl
    , let cheese' = addSlice cheese slice
    , let sl' = delete slice sl
    , fitsLater cheese' $ getMin sl'
    ]
  where
    fitsLater :: Cheese -> Slice -> Bool
    fitsLater (c1, c2, c3) (s1, s2) = (c1 <= s1) && (c2 <= s2)

sliceToState :: SliceMS -> Slice -> State
sliceToState slices slice = St (sliceToCheese slice, [slice], delete slice slices)

sliceToCheese :: Slice -> Cheese
sliceToCheese (s1, s2) = sortCheese (1, s1, s2)

solve :: [Slice] -> [Slice]
solve slices =
    (\(St (_, s, _)) -> reverse s)
        . head
        . (!! (length slices - 1))
        . iterate (ordNub . (>>= nextStates))
        $ [ sliceToState slices' slice
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
    putStrLn "Starte Berechnung..."
    startTime <- getCPUTime
    let !order = solve slices
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
