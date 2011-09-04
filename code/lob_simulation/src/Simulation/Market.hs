{-# LANGUAGE ExistentialQuantification #-}
module Simulation.Market where
  import Data.List
  import Simulation.Types
  import Simulation.Simulation
  import Simulation.LimitOrderBook
  import Simulation.Order hiding (Market)
  import Simulation.OrderResponse
  import Simulation.Trade
  import Control.Monad.Writer
  import Control.Monad.State
  import Data.Map (Map, (!))
  import qualified Data.Map as M

  data Market = Market {
    time :: Time,
    value :: Value,
    book :: LOB,
    lastBestBid :: Price,
    lastBestOffer :: Price,
    lastNumOrders :: Int
  }

  data MarketOutlook = CrossedRising | CrossedStable | CrossedFalling | Rising | Stable | Falling deriving (Eq, Show)


  data MarketMessage = Response Time OrderResponse | forall a b . (MarketSide a, OrderType b) => Message Time (Order a b)

  instance Message MarketMessage where
    messageTime      (Response t _) = t
    messageTime      (Message  t _) = t

  newtype TraderState = TraderState {
    inventoryLevel :: InventoryLevel 
  }

  makeMarket :: Value -> LOB -> Market
  makeMarket v b = Market 0 v b (getPrice v 0) (getPrice v 0) 0

  updateInventory :: TraderState -> InventoryLevel -> TraderState
  updateInventory (TraderState i) i' = TraderState $ i + i'

  data Sentiment = Calm | Choppy | Ramp | Toxic
  newtype Value = Value { getPrice :: Time -> Price }

  currentValue :: Market -> Price
  currentValue market = getPrice (value market) (time market)

  underlyingValue :: Sentiment -> Price -> Int -> Value
  underlyingValue sentiment initial ticks = Value (function sentiment initial)
    where function Calm   price tick = price
          function Choppy price tick = max (floor $ (sines !! tick + 1) * fromIntegral price) 0
          function Ramp   price tick = max (floor $ (rampDown !! tick) * fromIntegral price) 0
          function Toxic  price tick = if tick < 40 then price else 5
          sines     = cycle [sin x | x <- [0.0, (6*pi/fromIntegral ticks) .. 2*pi]]
          rampDown  = [1.0, 1.0-(1/fromIntegral ticks) ..]

  updateMarket :: Market -> [MarketMessage] -> SimState TraderState MarketMessage Market
  updateMarket last messages = do
    let tick                = time last
    let book'               = book last 
    let (book'', responses) = placeOrders tick book' messages
    tell $ map (Response tick) responses
    agentState <- get
    put $ M.mapWithKey (updateStateForAgent responses) agentState 
    return $  Market (tick + 1) (value last) book'' (bestBid book') (bestOffer book') (numOrders book')

  outlook :: Market -> MarketOutlook
  outlook market | bb >= bo  && bo >= cv  = CrossedRising
                 | bb >= cv  && cv >= bo = CrossedStable
                 | cv >= bb  && bb >= bo  = CrossedFalling
                 | bo >= bb  && bb >= cv  = Falling 
                 | bo >= cv  && cv >= bb = Stable
                 | cv >= bo  && bo >= bb  = Rising
                 where bb = bestBid $ book market
                       bo = bestOffer $ book market
                       cv = currentValue market

  updateStateForAgent :: [OrderResponse] -> AgentID -> TraderState -> TraderState
  updateStateForAgent responses id previous = foldl ((. inventoryDifference) . updateInventory) previous . filter (elem id . forTraders) $ responses
    where inventoryDifference (PenaltyResponse _ _) = 0
          inventoryDifference (TradeResponse t) | buySideTraderID t == id = tradedSize t
                                                | otherwise               = -(tradedSize t)

  placeOrders :: Time -> LOB -> [MarketMessage] -> (LOB, [OrderResponse])
  placeOrders time book messages = placeOrders' time (book, []) messages
    where placeOrders' _ (book, responses) []       = (book, responses)
          placeOrders' t (book, responses) (m:ms)   = placeOrders' t (book', responses ++ responses') ms
            where (book', responses') =  placeOrder' t book m
                  placeOrder' t b (Message time o) =  placeOrder t b o
