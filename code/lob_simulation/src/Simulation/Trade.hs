{-# LANGUAGE GADTs #-}
module Simulation.Trade (Trade(BuyTrade, SellTrade, CrossedTrade), makeTrade, buySideTraderID, sellSideTraderID, tradedSize, tradedPrice) where
  import Simulation.Types
  import Simulation.Order
 
  data Trade =
    BuyTrade     (Order Buy Market) (Order Sell Limit)  |
    SellTrade    (Order Buy Limit)  (Order Sell Market) |
    CrossedTrade (Order Buy Limit)  (Order Sell Limit)

  makeTrade :: (OrderType a, OrderType b) => (Order Buy a) -> (Order Sell b) -> Trade
  makeTrade buyOrder sellOrder = case (buyOrder,sellOrder) of
    ((BuyOrder _ _  ), (OfferOrder _ _ _)) -> BuyTrade     buyOrder sellOrder
    ((BidOrder _ _ _), (SellOrder _ _   )) -> SellTrade    buyOrder sellOrder
    ((BidOrder _ _ _), (OfferOrder _ _ _)) -> CrossedTrade buyOrder sellOrder

  buySideTraderID :: Trade -> TraderID
  buySideTraderID (BuyTrade     (BuyOrder t _)     (OfferOrder _ _ _)) = t
  buySideTraderID (SellTrade    (BidOrder t _ _)   (SellOrder  _ _))   = t
  buySideTraderID (CrossedTrade (BidOrder t _ _)   (OfferOrder _ _ _)) = t  

  sellSideTraderID :: Trade -> TraderID
  sellSideTraderID (BuyTrade     (BuyOrder _ _)     (OfferOrder t _ _)) = t
  sellSideTraderID (SellTrade    (BidOrder _ _ _)   (SellOrder  t _))   = t
  sellSideTraderID (CrossedTrade (BidOrder _ _ _)   (OfferOrder t _ _)) = t  

  tradedSize :: Trade -> Size
  tradedSize trade = uncurry min $ sizes
    where sizes :: (Size, Size)
          sizes = case trade of
            (BuyTrade     (BuyOrder _ bs)   (OfferOrder _ _ ss)) -> (bs, ss)
            (SellTrade    (BidOrder _ _ bs) (SellOrder  _ ss))   -> (bs, ss)
            (CrossedTrade (BidOrder _ _ bs) (OfferOrder _ _ ss)) -> (bs, ss)

  tradedPrice :: Trade -> Price
  tradedPrice (BuyTrade     (BuyOrder _ _)    (OfferOrder _ p _))  = p
  tradedPrice (SellTrade    (BidOrder _ p _)  (SellOrder  _ _))    = p
  tradedPrice (CrossedTrade (BidOrder _ bp _)  (OfferOrder _ sp _)) = (bp + sp) `div` 2
 
