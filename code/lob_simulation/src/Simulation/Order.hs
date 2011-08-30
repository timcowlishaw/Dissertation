{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances #-}

module Simulation.Order where
  import Simulation.Types  
  import Data.Maybe

  data Buy
  data Sell
  data Market
  data Limit

  class MarketSide a
  instance MarketSide Buy
  instance MarketSide Sell

  class OrderType a
  instance OrderType Market
  instance OrderType Limit

  data Order side orderType where
    BuyOrder    :: TraderID -> Size           -> Order Buy Market
    SellOrder   :: TraderID -> Size           -> Order Sell Market
    BidOrder    :: TraderID -> Price -> Size  -> Order Buy Limit
    OfferOrder  :: TraderID -> Price -> Size  -> Order Sell Limit 

  instance Show (Order Buy Market) where
    show (BuyOrder t1 s1) = "BuyOrder " ++ show t1 ++  " " ++ show s1

  instance Show (Order Sell Market) where
    show (SellOrder t1 s1) = "SellOrder " ++ show t1 ++ " " ++ show s1

  instance Show (Order Sell Limit) where
    show (OfferOrder t1 p1 s1) = "OfferOrder " ++ show t1 ++ " " ++ show p1 ++ " "++ show s1

  instance Show (Order Buy Limit) where
    show (BidOrder t1 p1 s1) = "BidOrder " ++ show t1 ++ " " ++ show p1 ++ " " ++ show s1

  instance Eq (Order Buy Market) where
    (BuyOrder t1 s1) == (BuyOrder t2 s2) = t1 == t2 && s1 == s2 

  instance Eq (Order Sell Market) where
    (SellOrder t1 s1) == (SellOrder t2 s2) = t1 == t2 && s1 == s2

  instance Eq (Order Buy Limit) where
    (BidOrder t1 p1 s1) == (BidOrder t2 p2 s2) = t1 == t2 && s1 == s2 && p1 == p2

  instance Eq (Order Sell Limit) where
    (OfferOrder t1 p1 s1) == (OfferOrder t2 p2 s2) = t1 == t2 && p1 == p2 && s1 == s2
   
  instance Ord (Order Buy Limit) where
    (BidOrder t1 p1 _) `compare` (BidOrder t2 p2 _) = p1 `compare` p2

  instance Ord (Order Sell Limit) where
    (OfferOrder t1 p1 _) `compare` (OfferOrder t2 p2 _) = p2 `compare` p1  

  type BuyOrder = Order Buy Market
  type SellOrder = Order Sell Market
  type OfferOrder = Order Sell Limit
  type BidOrder = Order Buy Limit
 
  buy = BuyOrder
  sell = SellOrder

  bid = BidOrder
  offer = OfferOrder

  for :: MarketSide s => (Size -> Order s Limit) -> Size -> Order s Limit
  f `for` a = f a

  traderID :: (Order s t) -> TraderID
  traderID (BuyOrder    t _)    = t
  traderID (SellOrder   t _)    = t
  traderID (BidOrder    t _ _)  = t
  traderID (OfferOrder  t _ _)  = t


  size :: (Order s t) -> Size
  size (BuyOrder    _ s)    = s
  size (SellOrder   _ s)    = s
  size (BidOrder    _ s _)  = s
  size (OfferOrder  _ s _)  = s
  
  price :: (Order s Limit) -> Price
  price (BidOrder   _ _ p) = p
  price (OfferOrder _ _ p) = p

  updateSize :: Size -> (Order s Limit) -> (Order s Limit)
  updateSize s (BidOrder    t _ p) = BidOrder t s p
  updateSize s (OfferOrder  t _ p) = OfferOrder t s p

