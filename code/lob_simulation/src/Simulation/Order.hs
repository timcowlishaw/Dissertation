{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances #-}

module Simulation.Order where
  import Simulation.Types  

  data Buy
  data Sell
  data Market
  data Limit

  data Order side orderType where
    BuyOrder  :: Size           -> Order Buy Market
    SellOrder :: Size           -> Order Sell Market
    BidOrder  :: Price -> Size  -> Order Buy Limit
    AskOrder  :: Price -> Size  -> Order Sell Limit 

  instance Show (Order Buy Market) where
    show (BuyOrder s1) = "BuyOrder " ++ show s1

  instance Show (Order Sell Market) where
    show (SellOrder s1) = "SellOrder " ++ show s1

  instance Show (Order Sell Limit) where
    show (AskOrder p1 s1) = "AskOrder " ++ show p1 ++ " "++ show s1

  instance Show (Order Buy Limit) where
    show (BidOrder p1 s1) = "BidOrder " ++ show p1 ++ " "++ show s1


  instance Eq (Order Buy Market) where
    (BuyOrder s1) == (BuyOrder s2) = s1 == s2

  instance Eq (Order Sell Market) where
    (SellOrder s1) == (SellOrder s2) = s1 == s2

  instance Eq (Order Buy Limit) where
    (BidOrder p1 s1) == (BidOrder p2 s2) = s1 == s2 && p1 == p2

  instance Eq (Order Sell Limit) where
    (AskOrder p1 s1) == (AskOrder p2 s2) = p1 == p2 && s1 == s2
   
  instance Ord (Order Buy Limit) where
    (BidOrder p1 _) `compare` (BidOrder p2 _) = p1 `compare` p2

  instance Ord (Order Sell Limit) where
    (AskOrder p1 _) `compare` (AskOrder p2 _) = p2 `compare` p1  

  type BuyOrder = Order Buy Market
  type SellOrder = Order Sell Market
  type AskOrder = Order Sell Limit
  type BidOrder = Order Buy Limit

  buy = BuyOrder
  sell = SellOrder

  bid = BidOrder
  ask = AskOrder

  for :: (Size -> Order s Limit) -> Size -> Order s Limit
  f `for` a = f a

  size :: (Order s t) -> Size
  size (BuyOrder s)   = s
  size (SellOrder s)  = s
  size (BidOrder s _) = s
  size (AskOrder s _) = s
 
  price :: (Order s Limit) -> Price
  price (BidOrder _ p) = p
  price (AskOrder _ p) = p 
