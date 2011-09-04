import Simulation.Simulation
import Simulation.Market hiding (book, value)
import Simulation.Order
import Simulation.Loggers
import Simulation.Market.LoggableInstances
import Simulation.Traders
import Simulation.Traders.InventoryFunctions
import Simulation.Traders.SensitivityFunctions
import Simulation.Traders.Types
import Simulation.LimitOrderBook
import Control.Monad
import Debug.Trace

ticks = 150

value = underlyingValue Choppy 2000 ticks

exponentialBackoffPenalty lobBefore lobAfter penaltyBefore = penaltyBefore + (floor . exp . fromIntegral $ liquidityTaken)
        where liquidityTaken = buySideLiquidity lobBefore + sellSideLiquidity lobBefore - buySideLiquidity lobAfter - sellSideLiquidity lobAfter

book = limitOrderBook `withBids`            ["fundamentalBuyer-1" `bids` 1500 `for` 500] 
                      `withOffers`          ["fundamentalSeller-1" `offers` 2500 `for` 500] 
                      `withPenaltyFunction` exponentialBackoffPenalty

intermediary = traderType "intermediary" highImbalanceSensitivity lowVolatilitySensitivity 
                        `withInitialInventory`  0
                        `withTargetInventory`   1200
                        `withTargetOrderSize`   200 
                        `withDemandBias`        0
                        `withOrderSizeLimit`    3000
                        `withInventoryFunction` intermediaryInventory 

hfTrader = traderType "hfTrader" highImbalanceSensitivity lowVolatilitySensitivity 
                        `withInitialInventory`  0 
                        `withTargetInventory`   800 
                        `withTargetOrderSize`   100 
                        `withDemandBias`        0 
                        `withOrderSizeLimit`    3000
                        `withInventoryFunction` hfInventory

fundamentalBuyer = traderType "fundamentalBuyer" lowImbalanceSensitivity highVolatilitySensitivity 
                        `withInitialInventory`  0
                        `withTargetInventory`   20000 
                        `withTargetOrderSize`   200 
                        `withDemandBias`        200 
                        `withOrderSizeLimit`    300 
                        `withInventoryFunction` fundamentalBuyerInventory

fundamentalSeller = traderType "fundamentalSeller" lowImbalanceSensitivity highVolatilitySensitivity 
                        `withInitialInventory`  0
                        `withTargetInventory`   100 
                        `withTargetOrderSize`   100 
                        `withDemandBias`        (-200) 
                        `withOrderSizeLimit`    3000 
                        `withInventoryFunction` fundamentalSellerInventory


opportunisticTrader = traderType "opportunisticTrader" mediumImbalanceSensitivity highVolatilitySensitivity 
                        `withInitialInventory`  0
                        `withTargetInventory`   800 
                        `withTargetOrderSize`   100 
                        `withDemandBias`        0 
                        `withOrderSizeLimit`    3000 
                        `withInventoryFunction` opportunisticTraderInventory

smallTrader = traderType "smallTrader" mediumImbalanceSensitivity highVolatilitySensitivity
                        `withInitialInventory`  0
                        `withTargetInventory`   400 
                        `withTargetOrderSize`   100 
                        `withDemandBias`        0 
                        `withOrderSizeLimit`    1 
                        `withInventoryFunction` smallTraderInventory

traders = [(11, intermediary),         -- 3 / 11
           (1, hfTrader),             -- 3 / 1
           (79, fundamentalBuyer),     -- 2 / 79
           (80, fundamentalSeller),    -- 2 / 80
           (363, opportunisticTrader),  -- 4 / 363
           (430, smallTrader)]          -- 4 / 430

loggers = [echoMessages, echoStates, logStates "states.csv", logMessages "messages.csv"]

main = runLoggers loggers =<< (runSim ticks $ Simulation (makeTraders traders) (makeMarket value book) updateMarket)
