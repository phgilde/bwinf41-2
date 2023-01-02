import Control.Monad
import Data.List
import Data.Maybe
import Data.Set qualified as Set
import SliceMultiSet qualified as SMS
import System.IO
import Text.Printf

type Slice = (Int, Int)

type Cheese = (Int, Int, Int)

newtype State = State (Cheese, [Slice], SMS.SliceMS)
instance Eq State where
    (State (_, su, sl)) == (State (_, su', sl')) = su == su' && sl == sl'

instance Ord State where
    (State (_, su, sl)) `compare` (State (_, su', sl')) = (su, sl) `compare` (su', sl')

ordNub :: Ord a => [a] -> [a]
ordNub l = ordNub' l Set.empty
  where
    ordNub' [] _ = []
    ordNub' (x : xs) s
        | x `Set.member` s = ordNub' xs s
        | otherwise = x : ordNub' xs (Set.insert x s)

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

addSlice :: Cheese -> Slice -> Cheese
addSlice (c1, c2, c3) (s1, s2)
    | c1 == s1 && c2 == s2 = (c1, c2, c3 + 1)
    | c1 == s1 && c3 == s2 = sortCheese (c1, c2 + 1, c3)
    | c2 == s1 && c3 == s2 = sortCheese (c1 + 1, c2, c3)

nextState :: State -> Slice -> State
nextState (State (c, s, s')) slice = State (addSlice c slice, slice : s, SMS.delete slice s')

nextStates :: State -> [State]
nextStates (State (c, s, s')) =
    filter (\(State (c, _, s)) -> all (fitsLater c) $ SMS.unique s)
        . map (nextState (State (c, s, s')))
        . sort
        $ SMS.findCompatible c s'

sliceToCheese :: Slice -> Cheese
sliceToCheese (s1, s2) = (1, s1, s2)

sliceToState :: SMS.SliceMS -> Slice -> State
sliceToState slices slice = State (sliceToCheese slice, [slice], SMS.delete slice slices)

slicesToStates :: [Slice] -> [State]
slicesToStates slices = ordNub $ map (sliceToState $ SMS.fromList slices) slices

findOrder :: [Slice] -> [Slice]
findOrder slices =
    let State (_, order, _) =
            head
                . (!! (length slices - 1))
                . iterate (ordNub . (>>= nextStates))
                . slicesToStates
                $ sort slices
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
    mapM_
        (\((s1, s2), (c1, c2, c3)) -> putStrLn $ printf "Scheibe: %d %d, Käse: %d %d %d" s1 s2 c1 c2 c3)
        (zip order (slicesToCheeses order))