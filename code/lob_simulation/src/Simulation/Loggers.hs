{-# LANGUAGE GADTs #-}
module Simulation.Loggers where
  import Simulation.Simulation
  import Simulation.LimitOrderBook
  import Simulation.Trade
  import Simulation.Order hiding (Market)
  import Simulation.OrderResponse
  import Simulation.Market
  import Text.CSV
  import Control.Applicative
  import Control.Monad
 
  class Loggable a where
    output :: a -> Record
  

  instance Loggable Market where
    output = ([show . time,
            show . outlook,
            show . currentValue,
            show . bestBid . book,
            show . bestOffer . book,
            show . buySideLiquidity . book,
            show . sellSideLiquidity . book,
            show . buySideDepth . book,
            show . sellSideDepth . book,
            show . flip buySideDepthNearTop 0.05 . book,
            show . flip sellSideDepthNearTop 0.05 . book] <*>) . return

  instance Loggable MarketMessage where
    output m = case m of
      (Response time (TradeResponse t))              -> [show time, "Trade",   buySideTraderID t, sellSideTraderID t, show $ tradedSize t, show $ tradedPrice t, none]
      (Response time (PenaltyResponse id p))         -> [show time, "Penalty", id,                none,               none,                none,                 show p]
      (Message  time (BuyOrder id quantity))         -> [show time, "Buy",     id,                none,               show quantity,       none,                 none]
      (Message  time (SellOrder id quantity))        -> [show time, "Sell",    id,                none,               show quantity,       none,                 none]
      (Message  time (BidOrder id price quantity))   -> [show time, "Bid",     id,                none,               show quantity,       show price,           none]
      (Message  time (OfferOrder id price quantity)) -> [show time, "Offer",   id,                none,               show quantity,       show price,           none]

  none = ""

  echoStates :: SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  echoStates result = do
    mapM_ (putStrLn . ("State: " ++) . show . output) . states $ result
    return result

  echoMessages :: SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  echoMessages result = do
    mapM_ (putStrLn . ("Message: " ++) . show . output) . messages $ result
    return result

  logStates :: FilePath -> SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  logStates filename result = do
    let csv  = printCSV . map output . states $ result
    writeFile filename csv
    return result
  
  logMessages :: FilePath -> SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  logMessages filename result = do
    let csv = printCSV . map output . messages $ result
    writeFile filename csv
    return result

