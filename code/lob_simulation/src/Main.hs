import Simulation.Simulation
import Simulation.Market
import Simulation.Order
import Simulation.Loggers
import Simulation.Traders
import Simulation.LimitOrderBook
import Control.Monad

main :: IO ()
main = (defaultMain <=< loggers) =<< (runSim 200 $ Simulation traders market updateMarket)
  where market  = makeMarket value' book'
        value'   = underlyingValue Calm 2000 
        book'    = empty `withTrades` initialBids `withTrades` initialOffers
        traders = makeTraders 3  intermediary ++           --11 -- 3
                  makeTraders 3  hfTrader ++               --1  -- 3
                  makeTraders 2  fundamentalBuyer ++       --79 -- 2
                  makeTraders 2  fundamentalSeller ++      --80 -- 2
                  makeTraders 4  opportunisticTrader ++    --363 -- 4
                  makeTraders 4  smallTrader               --430 -- 4
        initialBids = ["fundamentalBuyer-1" `bids` 1500 `for` 500]
        initialOffers = ["fundamentalSeller-1" `offers` 2500 `for` 500]
        loggers = echoStates >=> echoMessages >=> logStates "states.csv" >=> logMessages "messages.csv"
