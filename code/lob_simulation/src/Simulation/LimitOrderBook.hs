module Simulation.LimitOrderBook where
  import Simulation.Types
  import Simulation.Order
  import Simulation.MultiLevelList (MultilevelList)
  import qualified Simulation.MultiLevelList as MLL

  data TraderInfoItem = TraderInfo { traderID :: TraderID, liquidityTaken :: Double, penaltyApplied :: Int, time :: Time}

  data LOB :: {
    bids :: MultiLevelList (Order Buy Limit),
    offers :: MultiLevelList (Order Sell Limit),
    buysWaiting :: [Order Buy Market],
    sellsWaiting :: [Order Sell Market],
    tradesDone :: [Trade],
    time :: Time,
    buysReceived :: [Order Buy Market],
    sellsReceived :: [Order Sell Market]
    sentiment :: Sentiment
    traderInfo :: [TraderInfoItem]
  }

  empty = LOB emptyMultiLevelList emptyMultiLevelList [] [] [] 0 [] [] Calm [] initialStats

  primed = foldL (flip match) emptyOrderList [createOrder 2050 500 0 Phantom Offer, createOrder 1950 500 0 Phantom Bid] 

  emptyTraderInfoItem = TraderInfo Phantom 0 0 0

  incrementTime lob = (executeTrades lob) { time = (time lob) + 1}

  executeTrades = fail "Not implemented yet"

  bestBid = head . MLL.first . bids

  bestOffer = head . MLL.first . offers 
  
  buySideLiquidity :: LOB -> Int

  sellSideLiquidity :: LOB -> Int

  buySideDepth :: LOB -> Int 

  sellSideDepth :: LOB -> Int

  buySideDepthNearTop :: LOB -> Int

  sellSideDepthNearTop :: LOB -> Int

  buySideLevels :: LOB -> Int
  
  sellSideLevels :: LOB -> Int

  midPrice :: LOB -> Price

  lastTradedPrice :: LOB -> Price

  placeBid :: LOB -> (Order Buy Limit) -> LOB

  placeOffer :: LOB -> (Order Sell Limit) -> LOB

  placeBuy :: LOB -> (Order Buy Market) -> LOB

  placeSell :: LOB -> Order Sell Market) -> LOB

