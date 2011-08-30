module Simulation.OrderResponse where
  import Simulation.Types
  import Simulation.Trade
  import Control.Applicative
  
  data OrderResponse = TradeResponse Trade | PenaltyResponse TraderID Penalty

  forTraders :: OrderResponse -> [TraderID]
  forTraders (TradeResponse trade)      = [buySideTraderID, sellSideTraderID] <*> (return trade)
  forTraders (PenaltyResponse trader _) = [trader]
