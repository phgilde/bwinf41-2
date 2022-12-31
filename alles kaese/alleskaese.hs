import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import System.IO

type Slice = (Integer, Integer)

type Cheese = (Integer, Integer, Integer)

fitsLater :: Cheese -> Slice -> Bool
fitsLater (c1, c2, c3) (s1, s2) = (c1 <= s1) && (c2 <= s2)

fitsNext :: Cheese -> Slice -> Bool
fitsNext (c1, c2, c3) (s1, s2) = (c1 == s1 && c2 == s2) || (c1 == s1 && c3 == s2) || (c2 == s1 && c3 == s2)

sortCheese :: Cheese -> Cheese
sortCheese (c1, c2, c3)
    | c1 <= c2 && c2 <= c3 = (c1, c2, c3)
    | c1 <= c3 && c3 <= c2 = (c1, c3, c2)
    | c2 <= c1 && c1 <= c3 = (c2, c1, c3)

sortSlice :: Slice -> Slice
sortSlice (s1, s2)
    | s1 <= s2 = (s1, s2)
    | otherwise = (s2, s1)

type State = (Cheese, [Slice], [Slice])

addSlice :: Cheese -> Slice -> Cheese
addSlice (c1, c2, c3) (s1, s2)
    | c1 == s1 && c2 == s2 = sortCheese (c1, c2, c3 + 1)
    | c1 == s1 && c3 == s2 = sortCheese (c1, c2 + 1, c3)
    | c2 == s1 && c3 == s2 = sortCheese (c1 + 1, c2, c3)

nextState :: State -> Slice -> Maybe State
nextState (c, s, s') slice
    | fitsNext c slice = Just (addSlice c slice, slice : s, delete slice s')
    | otherwise = Nothing

nextStates :: State -> [State]
nextStates (c, s, s') = filter (\(c, _, s) -> all (fitsLater c) s) $ mapMaybe (nextState (c, s, s')) s'

sliceToCheese :: Slice -> Cheese
sliceToCheese (s1, s2) = (1, s1, s2)

sliceToState :: [Slice] -> Slice -> State
sliceToState slices slice = (sliceToCheese slice, [slice], delete slice slices)

slicesToStates :: [Slice] -> [State]
slicesToStates slices = map (sliceToState slices) slices

findOrder :: [Slice] -> [Slice]
findOrder slices =
    let (_, order, _) = head $ iterate (>>= nextStates) (slicesToStates slices) !! (length slices - 1)
     in reverse order

slicesToCheeses :: [Slice] -> [Cheese]
slicesToCheeses (slice : slices) = scanl addSlice (sliceToCheese slice) slices

sliceFromString :: String -> Slice
sliceFromString s = (read s1, read s2)
    where
        [s1, s2] = words s

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
    slices <- readSlices path
    let order = findOrder slices
    print $ length order
    mapM_ (\((s1, s2), (c1, c2, c3)) -> putStrLn $ printf "Scheibe: %d %d, Käse: %d %d %d" s1 s2 c1 c2 c3) 
        (zip order (slicesToCheeses order))