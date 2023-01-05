module SliceMultiSet (
    SliceMS,
    Slice,
    Cheese,
    insert,
    delete,
    fromList,
    findCompatible,
    unique,
    getLowestEach,
    getHighestEach,
    getMax,
    member,
) where

import Data.List qualified as List

import Data.IntMap.Lazy qualified as Map
import IntMultiSet qualified as MS

type Slice = (Int, Int)
type Cheese = (Int, Int, Int)

type SliceMS = Map.IntMap MS.IntMultiSet

insert :: Slice -> SliceMS -> SliceMS
insert (s, s') m
    | s `Map.member` m = Map.adjust (MS.insert s') s m
    | otherwise = insert (s, s') $ Map.insert s MS.empty m

delete :: Slice -> SliceMS -> SliceMS
delete (s, s') =
    Map.update
        ( \ms ->
            let deleted = MS.delete s' ms
             in if deleted /= MS.empty then Just deleted else Nothing
        )
        s

fromList :: [Slice] -> SliceMS
fromList = foldr insert Map.empty

findCompatible :: Cheese -> SliceMS -> [Slice]
findCompatible (c1, c2, c3) m =
    List.nub
        [(c1, c2) | c1 `Map.member` m, c2 `MS.member` (m Map.! c1)]
        ++ [(c1, c3) | c1 `Map.member` m, c3 `MS.member` (m Map.! c1)]
        ++ [(c2, c3) | c2 `Map.member` m, c3 `MS.member` (m Map.! c2)]

unique :: SliceMS -> [Slice]
unique = concatMap (\(s, s') -> map (s,) $ MS.unique s') . Map.toList

getLowestEach :: SliceMS -> [Slice]
getLowestEach = map (\(s, s') -> (s, MS.findMin s')) . Map.toList

getHighestEach :: SliceMS -> [Slice]
getHighestEach = map (\(s, s') -> (s, MS.findMax s')) . Map.toList

getMax :: SliceMS -> Slice
getMax m
    | Map.null m = (0, 0)
    | otherwise = let (s, s') = Map.findMax m in (s, MS.findMax s')

member :: Slice -> SliceMS -> Bool
member (s, s') m = s `Map.member` m && s' `MS.member` (m Map.! s)