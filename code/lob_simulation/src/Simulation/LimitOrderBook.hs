{-# LANGUAGE GADTs #-}
module Simulation.LimitOrderBook(LOB(), limitOrderBook, noPenalty, withBids, withOffers, withPenaltyFunction, bestBid, numOrders, bestOffer, buySideLiquidity, sellSideLiquidity, buySideDepthNearTop, sellSideDepthNearTop, buySideDepth, sellSideDepth,buySideLevels, sellSideLevels, midPrice, lastTradedPrice, placeOrder, isCrossed) where
  import Simulation.Types
  import Simulation.Order hiding (bids, offers)
  import Simulation.OrderResponse
  import Simulation.Trade
  --import Simulation.Traders
  import Simulation.LimitOrderList hiding (empty, insert, numOrders)
  import qualified Simulation.LimitOrderList as LOL (empty, insert, numOrders)
  import Data.Map (Map)
  import qualified Data.Map as M
  import Control.Applicative hiding (empty)
  import Safe
  import Data.Maybe (fromMaybe)
  import Debug.Trace

  type PenaltyFunction = LOB -> LOB -> Penalty -> Penalty

  data LOB = LOB {
    bids :: LimitOrderList Buy,
    offers :: LimitOrderList Sell,
    tradesDone :: [Trade],
    penalties :: Map TraderID Penalty,
    penaltyFunction :: PenaltyFunction
  }
 
  noPenalty = (flip $ const . flip const)

  limitOrderBook = LOB LOL.empty LOL.empty [] M.empty noPenalty 

  withBids :: LOB -> [Order Buy Limit] -> LOB
  withBids book orders = foldl place book orders
    where place book order = book { bids   = LOL.insert (bids book) order }

  withOffers :: LOB -> [Order Sell Limit] -> LOB
  withOffers book orders = foldl place book orders
    where place book order = book { offers = LOL.insert (offers book) order}

  withPenaltyFunction :: LOB -> PenaltyFunction -> LOB
  withPenaltyFunction book function = book {penaltyFunction = function}

  bestBid :: LOB -> Price
  bestBid = bestPrice . bids

  bestOffer :: LOB -> Price
  bestOffer = bestPrice . offers

  numOrders :: LOB -> Int
  numOrders book = numBids book + numOffers book

  numBids :: LOB -> Int
  numBids = LOL.numOrders . bids

  numOffers :: LOB -> Int
  numOffers = LOL.numOrders . offers

  buySideLiquidity :: LOB -> Int
  buySideLiquidity = liquidity . bids

  sellSideLiquidity :: LOB -> Int
  sellSideLiquidity = liquidity . offers

  buySideDepth :: LOB -> Int 
  buySideDepth = depth . bids

  sellSideDepth :: LOB -> Int
  sellSideDepth = depth . offers

  buySideDepthNearTop :: LOB -> Double -> Int
  buySideDepthNearTop = depthNearTop . bids 

  sellSideDepthNearTop :: LOB -> Double -> Int
  sellSideDepthNearTop = depthNearTop . offers

  buySideLevels :: LOB -> Int
  buySideLevels = numLevels . bids 
 
  sellSideLevels :: LOB -> Int
  sellSideLevels = numLevels . offers

  midPrice :: LOB -> Price
  midPrice book = floor $ fromIntegral (bestBid book + bestOffer book) / 2 
 
  lastTradedPrice :: LOB -> Price
  lastTradedPrice = fromMaybe 0 . fmap tradedPrice . headMay . tradesDone

  placeOrder :: (MarketSide a, OrderType b) => Time -> LOB -> Order a b -> (LOB, [OrderResponse])
  placeOrder tick book order | tick <= penaltyForTrader  = (book,  [PenaltyResponse trader penaltyForTrader])
                             | otherwise                 = (book'', map TradeResponse trades'')
    where trader            = traderID order
          penaltyForTrader  = M.findWithDefault 0 trader (penalties book)
          trades''          = trades ++ trades'
          (book'', trades') = uncrossBook $ book' {penalties = penalties'}
          penalties'        = M.insert trader newPenalty (penalties book) 
          newPenalty        = penaltyFunction book book book' penaltyForTrader
          (book', trades)   = bookAndTrades
          bookAndTrades :: (LOB, [Trade])
          bookAndTrades     = case order of
            (BidOrder   _ _ _) ->  (book { bids   = LOL.insert  (bids   book) order}, [])
            (OfferOrder _ _ _) ->  (book { offers = LOL.insert  (offers book) order}, [])
            (BuyOrder   _ _) ->  executeBuy  order book
            (SellOrder  _ _) ->  executeSell order book
   
  executeBuy :: (Order Buy Market) -> LOB -> (LOB, [Trade])
  executeBuy order book = (book', trades) 
    where book'                  = book {offers = offers', tradesDone = trades}
          (offers', offerOrders) = toFill (offers book) order
          trades                 = map (makeTrade order) offerOrders ++ tradesDone book


  executeSell :: (Order Sell Market) -> LOB -> (LOB, [Trade])
  executeSell order book = (book', trades)
    where book'              = book {bids = bids', tradesDone = trades}
          (bids', bidOrders) = toFill (bids book) order
          trades             = map (flip makeTrade order) bidOrders ++ tradesDone book

  isCrossed :: LOB -> Bool
  isCrossed book = (not . elem 0 $ [numBids, numOffers] <*> return book) && bestBid book > bestOffer book

  uncrossBook :: LOB -> (LOB, [Trade])
  uncrossBook book = uncrossBook' (book, [])
    where uncrossBook' (b,t) | isCrossed b = uncrossBook' (b', trades' ++ t)
                             | otherwise   =  (b, t)
            where (bid, bids')       = popBest (bids book)
                  (offer, offers')    = popBest (offers book)
                  trades              = [makeTrade bid offer] 
                  trades'             = trades ++ tradesDone book
                  b'                  = b { bids = bids', offers = offers', tradesDone = trades'}
