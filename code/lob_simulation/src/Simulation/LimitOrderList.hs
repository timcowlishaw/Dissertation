{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ViewPatterns #-}
module Simulation.LimitOrderList (LimitOrderList(), empty, isEmpty, bestPrice, numOrders, bestOrders, worstPrice, worstOrders, insert, delete, liquidity, depth, depthNearTop,  numLevels, toFill, popBest) where
  import Prelude hiding (last, length, filter, zip, scanl, drop, splitAt, concat, sum, null, foldl)
  import qualified Data.List as L
  import Data.List ((\\))
  import Data.Maybe (fromMaybe, listToMaybe)
  import Simulation.Types
  import Simulation.Order
  import Data.Sequence hiding (null, empty, length)
  import qualified Data.Sequence as S (null, empty, length)
  import Data.Foldable

  data OrderListLevel a = OrderListLevel {levelPrice :: Price, levelOrders :: Seq (Order a Limit)}

  instance Eq (OrderListLevel a) where
    oll1 == oll2 = levelPrice oll1 == levelPrice oll2

  instance Ord (OrderListLevel Buy) where
    compare oll1 oll2 = levelPrice oll1 `compare` levelPrice oll2

  instance Ord (OrderListLevel Sell) where
    compare oll1 oll2 = invert $ levelPrice oll1 `compare` levelPrice oll2
                        where invert LT = GT
                              invert GT = LT
                              invert EQ = EQ 

  newtype LimitOrderList a = LimitOrderList { levels :: [OrderListLevel a] }

  instance (Show (Order a Limit)) => Show (LimitOrderList a) where
    show = ("LimitOrderList" ++) . show . map (show . levelOrders) . levels

  empty :: MarketSide a => LimitOrderList a
  empty = LimitOrderList []

  isEmpty :: MarketSide a => LimitOrderList a -> Bool
  isEmpty = L.null . levels 

  numOrders :: MarketSide a => LimitOrderList a -> Int
  numOrders = L.length . orders

  orders :: MarketSide a => LimitOrderList a -> [Order a Limit]
  orders = concat . map ordersForLevel . levels

  bestPrice :: MarketSide a => LimitOrderList a -> Price
  bestPrice = levelPrice . head . levels

  bestOrders :: MarketSide a => LimitOrderList a -> [Order a Limit]
  bestOrders = toList . levelOrders . head . levels
  
  popBest :: (Ord (OrderListLevel a), MarketSide a) => LimitOrderList a -> (Order a Limit, LimitOrderList a)
  popBest orderlist       = (order, orderlist')
    where orderlist'      = LimitOrderList $ L.insert level' (L.delete level $ levels orderlist)
          level           = head . levels $ orderlist
          (order:orders)  = ordersForLevel level
          level'          = level { levelOrders = fromReversedList orders } 

  worstPrice :: MarketSide a => LimitOrderList a -> Price
  worstPrice = levelPrice . L.last . levels

  worstOrders :: MarketSide a => LimitOrderList a -> [Order a Limit]
  worstOrders = toList . levelOrders . L.last . levels

  insert :: (Ord (OrderListLevel a), MarketSide a) => LimitOrderList a -> Order a Limit -> LimitOrderList a
  insert orderList order = LimitOrderList $ L.insert level' (L.delete level $ levels orderList)
    where level = levelFor orderList order
          level' = OrderListLevel (levelPrice level) (order <| levelOrders level)

  delete :: (Eq (Order a Limit), Ord (OrderListLevel a), MarketSide a) => LimitOrderList a -> Order a Limit -> LimitOrderList a
  delete orderList order = LimitOrderList levels''
    where level   = levelFor orderList order
          level'  = OrderListLevel (levelPrice level) (seqDelete order $ levelOrders level)
          levels' = L.delete level (levels orderList)
          levels'' | S.null $ levelOrders level' = levels'
                   | otherwise                   = L.insert level' levels'

  liquidity :: MarketSide a => LimitOrderList a -> Size
  liquidity = sum . map size . orders
  
  depth :: MarketSide a => LimitOrderList a -> Size
  depth = sum . map size . bestOrders

  depthNearTop :: MarketSide a => LimitOrderList a -> Double -> Size
  depthNearTop list percent = sum . map size . ordersInTop list $ percent
  
  numLevels :: MarketSide a => LimitOrderList a -> Int
  numLevels = L.length . levels

  toFill :: (Eq (Order a Limit), Ord (OrderListLevel a), MarketSide a, MarketSide b, OrderType c) => LimitOrderList a -> Order b c -> (LimitOrderList a, [Order a Limit])
  toFill list order = (list'', matchingOrders)
    where list'           = foldl delete list matchingOrders
          list''          = fromMaybe list' $ insert list' `fmap` partialOrder'
          partialOrder    = orderPartiallyFilledByOrder list order
          partialOrder'   = updateSize partialFillSize `fmap` partialOrder
          partialFillSize = size order - (sum . map size . ordersTotallyFilledByOrder list $ order)
          matchingOrders  = ordersMatchingOrder list order

  levelFor :: (Ord (OrderListLevel a), MarketSide a) => LimitOrderList a -> Order a Limit -> OrderListLevel a
  levelFor list order = fromMaybe (OrderListLevel (price order) S.empty) $ L.find ((== price order) . levelPrice) . levels $ list
 
  ordersForLevel :: MarketSide a => OrderListLevel a -> [Order a Limit]
  ordersForLevel = listSeqFromRight . levelOrders
    where listSeqFromRight (viewr -> EmptyR) = []
          listSeqFromRight (viewr -> xs :> x) = x:listSeqFromRight(xs)
 
  ordersInTop :: MarketSide a => LimitOrderList a -> Double -> [Order a Limit]
  ordersInTop list percent = concat . map (toList . levelOrders) . L.filter (inTop percent) . levels $ list
    where inTop :: Double -> (OrderListLevel a -> Bool)
          inTop p | spread > 0 = (>= (fromIntegral . floor $ bottomPrice + spread*p)) . levelPrice
                  | otherwise  = (<= (fromIntegral . floor $ bottomPrice + (1-p)*abs spread)) . levelPrice
            where spread = fromIntegral $ bestPrice list - worstPrice list
                  bottomPrice = fromIntegral . worstPrice $ list
                        
  ordersMatchingOrder :: (MarketSide a, MarketSide b, OrderType c) => LimitOrderList a -> Order b c -> [Order a Limit]
  ordersMatchingOrder list order = takeWhileAccumulating (>=0-s) (\s o -> s - size o) s os  
    where s  = size order
          os = orders list

  ordersTotallyFilledByOrder :: (MarketSide a, MarketSide b, OrderType c) => LimitOrderList a -> Order b c -> [Order a Limit]
  ordersTotallyFilledByOrder list order = takeWhileAccumulating (>=0) (\s o -> s - size o) s os  
    where s = size order
          os = orders list
  
  orderPartiallyFilledByOrder :: (Eq (Order a Limit), MarketSide a, MarketSide b, OrderType c) => LimitOrderList a -> Order b c -> Maybe (Order a Limit) 
  orderPartiallyFilledByOrder list order = listToMaybe $ ordersMatchingOrder list order \\ ordersTotallyFilledByOrder list order
  

  takeWhileAccumulating :: (b -> Bool) -> (b -> a -> b)-> b -> [a] -> [a]
  takeWhileAccumulating predicate accumulate init list = map fst . L.takeWhile (predicate . snd) . L.zip list . tail . L.scanl accumulate init $ list


  fromReversedList :: [a] -> Seq a
  fromReversedList =  L.foldl (flip (<|)) S.empty

  seqDelete :: (Eq a) => a -> Seq a -> Seq a 
  seqDelete x xs = fromMaybe xs deleted
    where deleted     = (joinOthers . splitSeqAt) `fmap` pos
          splitSeqAt  = flip Data.Sequence.splitAt xs                 
          joinOthers  = uncurry ((. Data.Sequence.drop 1) . (><))
          pos         = L.elemIndex x . toList $ xs
            
