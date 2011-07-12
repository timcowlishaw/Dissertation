module ABSSketch.Order where
  import ABSSketch.Timing
  import ABSSketch.Traders
  type Price = Int

  data OrderType = Bid | Offer | Sell | Buy | None | Abort deriving (Eq, Show) 
  
  data Order = Order {
    price :: Price,
    size :: Int,
    time :: Tick,
    trader :: Trader,
    orderType :: OrderType
  } deriving (Eq, Show)
