module Simulation.MultiLevelList (MultiLevelList, empty, isEmpty, first, last, insert, delete) where
  import Prelude hiding (last)
  import qualified Data.List as L
  import Data.Maybe (fromMaybe)

  newtype (Ord a) => ListLevel a = ListLevel {items :: [a]} deriving (Eq)

  instance (Ord a) => Ord (ListLevel a) where
    compare (ListLevel [])       (ListLevel [])       = EQ
    compare (ListLevel [])       _                    = LT
    compare _                    (ListLevel [])       = GT
    compare (ListLevel (o1:os1)) (ListLevel (o2:os2)) = o1 `compare` o2

  newtype (Ord a) => MultiLevelList a = MultiLevelList [ListLevel a] deriving (Eq)
  
  instance (Ord a, Show a) => Show (MultiLevelList a) where
    show (MultiLevelList ls) =  ("MultiLevelList" ++) . show $ map (show . items) ls
  

  empty :: (Ord a) => MultiLevelList a
  empty = MultiLevelList []

  isEmpty :: (Ord a) => MultiLevelList a -> Bool
  isEmpty ol | ol == empty = True
             | otherwise   = False 

  first :: (Ord a) => MultiLevelList a -> [a]
  first (MultiLevelList ols) = items . head $ ols

  last :: (Ord a) => MultiLevelList a -> [a]
  last (MultiLevelList ols) = items . L.last $ ols

  levelFor :: (Ord a) => a -> MultiLevelList a -> ListLevel a
  levelFor x (MultiLevelList xs) = fromMaybe (ListLevel []) $ L.find ((== x) . head . items) xs

  insert :: (Ord a) => a -> MultiLevelList a -> MultiLevelList a
  insert o mll@(MultiLevelList ls) = MultiLevelList $ L.insert (level') (L.delete level ls)
                              where level = levelFor o mll
                                    level' = ListLevel $ o : items level

  delete :: (Ord a) => a -> MultiLevelList a -> MultiLevelList a
  delete o ol@(MultiLevelList os) = MultiLevelList os''
                                where os'    = L.delete level os
                                      level  = levelFor o ol
                                      level' = ListLevel . L.delete o . items $ level
                                      os''   | L.null . items $ level' = os'
                                             | otherwise               = L.insert level' os'

{- These should go in LOB
    firstPrice :: MultiLevelList -> Price

    lastPrice :: MultiLevelList -> Price

    liquidity :: MultiLevelList -> Size

    levels :: MultiLevelList -> Int

    depth :: MultiLevelList -> Int

    depthNearTop :: MultiLevelList -> Int

    popLast :: MultiLevelList -> (Order, MultiLevelList)

    fill :: MultiLevelList -> Order -> [Trade] -> (MultiLevelList -> Order -> [Trade])

    fillCross :: MultiLevelList -> Order -> [Trade] -> (MultiLevelList -> Order -> [Trade])
-}

