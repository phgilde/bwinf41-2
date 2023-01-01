module SliceMultiSet (
    SliceMS,
    Slice,
    Cheese,
    insert,
    delete,
    fromList,
    findCompatible,
    unique,
) where

import qualified Data.IntMap.Lazy as Map
import MultiSet qualified as MS

type Slice = (Int, Int)
type Cheese = (Int, Int, Int)

type SliceMS = Map.IntMap (MS.MultiSet Int)

insert :: Slice -> SliceMS -> SliceMS
insert (s, s') m
    | s `Map.member` m = Map.adjust (MS.insert s') s m
    | otherwise = insert (s, s') $ Map.insert s MS.empty m

delete :: Slice -> SliceMS -> SliceMS
delete (s, s') = Map.adjust (MS.delete s') s

fromList :: [Slice] -> SliceMS
fromList = foldr insert Map.empty

findCompatible :: Cheese -> SliceMS -> [Slice]
findCompatible (c1, c2, c3) m =
    [(c1, c2) | c1 `Map.member` m, c2 `MS.member` (m Map.! c1)]
        ++ [(c1, c3) | c1 `Map.member` m, c3 `MS.member` (m Map.! c1)]
        ++ [(c2, c3) | c2 `Map.member` m, c3 `MS.member` (m Map.! c2)]

unique :: SliceMS -> [Slice]
unique = concatMap (\(s, s') -> map (s,) $ MS.unique s') . Map.toList