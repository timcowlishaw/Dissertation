{-# LANGUAGE GADTs #-}
module Simulation.Market.LoggableInstances where

  import Simulation.Loggers
  import Simulation.Market
  import Simulation.LimitOrderBook
  import Simulation.Trade
  import Simulation.Order hiding (Market)
  import Simulation.OrderResponse

  instance Loggable Market where
    output m = [
            ("time",                    show . time $ m),
            ("outlook",                 show . outlook $ m),
            ("currentValue",            show . currentValue $ m),
            ("bestBid",                 show . bestBid . book $ m),
            ("bestOffer",               show . bestOffer . book $ m),
            ("buySideLiquidity",        show . buySideLiquidity . book $ m),
            ("sellSideLiquitity",       show . sellSideLiquidity . book $ m),
            ("buySideDepth",            show . buySideDepth . book $ m),
            ("sellSideDepth",           show . sellSideDepth . book $ m),
            ("buySideDepthNearTop",     show . flip buySideDepthNearTop 0.05 . book $ m),
            ("sellSideDepthNearTop",    show . flip sellSideDepthNearTop 0.05 . book $ m),
            ("midPrice",                show . midPrice . book $ m),
            ("lastTradedPrice",         show . lastTradedPrice . book $ m)]

  instance Loggable MarketMessage where
    output m = zip labels values
      where labels = ["time", "type", "buySideTrader", "sellSideTrader", "size", "price", "penalty"]
            values :: [String]
            values = case m of
              (Response time (TradeResponse t))              -> [show time, "Trade",   buySideTraderID t, sellSideTraderID t, show $ tradedSize t, show $ tradedPrice t, none]
              (Response time (PenaltyResponse id p))         -> [show time, "Penalty", id,                none,               none,                none,                 show p]
              (Message  time (BuyOrder id quantity))         -> [show time, "Buy",     id,                none,               show quantity,       none,                 none]
              (Message  time (SellOrder id quantity))        -> [show time, "Sell",    id,                none,               show quantity,       none,                 none]
              (Message  time (BidOrder id price quantity))   -> [show time, "Bid",     id,                none,               show quantity,       show price,           none]
              (Message  time (OfferOrder id price quantity)) -> [show time, "Offer",   id,                none,               show quantity,       show price,           none]

