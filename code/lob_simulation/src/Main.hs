import Simulation.Simulation
import Simulation.Market
import Simulation.Loggers
import Simulation.Traders
import Simulation.LimitOrderBook
import Control.Monad

main = defaultMain $ loggers =<< (runSim 2 $ Simulation traders market updateMarket)
  where market  = makeMarket value book
        value   = underlyingValue Calm 2000 
        book    = empty
        traders = makeTraders 3  intermediary ++           --11
                  makeTraders 3  hfTrader ++               --1    
                  makeTraders 2  fundamentalBuyer ++       --79
                  makeTraders 2  fundamentalSeller ++      --80
                  makeTraders 4  opportunisticTrader ++    --363
                  makeTraders 4  smallTrader               --430
        loggers = echoStates >=> logStates "states.csv"
