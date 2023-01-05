module IntMultiSet (
    IntMultiSet,
    insert,
    fromList,
    delete,
    unique,
    member,
    empty,
    findMin,
    findMax,
) where

import Data.IntMap qualified as Map

type IntMultiSet = (Map.IntMap Int)

insert ::  Int -> IntMultiSet -> IntMultiSet
insert x = Map.insertWith (+) x 1

fromList :: [Int] -> IntMultiSet
fromList = foldr insert Map.empty

delete :: Int -> IntMultiSet -> IntMultiSet
delete = Map.update (\v -> if v == 1 then Nothing else Just (v - 1))

unique :: IntMultiSet -> [Int]
unique = Map.keys

member :: Int -> IntMultiSet -> Bool
member = Map.member

empty :: IntMultiSet
empty = Map.empty

findMin :: IntMultiSet -> Int
findMin = fst . Map.findMin

findMax :: IntMultiSet -> Int
findMax = fst . Map.findMax