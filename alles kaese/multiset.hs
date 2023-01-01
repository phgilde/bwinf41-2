module MultiSet (
    MultiSet,
    insert,
    fromList,
    delete,
    unique,
    member,
    empty
) where

import Data.Map qualified as Map

type MultiSet a = (Map.Map a Int)

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x = Map.insertWith (+) x 1

fromList :: Ord a => [a] -> MultiSet a
fromList = foldr insert Map.empty

delete :: Ord a => a -> MultiSet a -> MultiSet a
delete = Map.update (\v -> if v == 1 then Nothing else Just (v - 1))

unique :: Ord a => MultiSet a -> [a]
unique = Map.keys

member :: Ord a => a -> MultiSet a -> Bool
member = Map.member

empty :: MultiSet a
empty = Map.empty