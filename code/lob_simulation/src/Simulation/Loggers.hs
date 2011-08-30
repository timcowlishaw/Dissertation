module Simulation.Loggers where
  import Simulation.Simulation
  import Simulation.LimitOrderBook
  import Simulation.Market
  import Text.CSV
  import Control.Applicative
  import Control.Monad
 
  class Loggable a where
    output :: a -> Record
  

  instance Loggable Market where
    output = ([show . time, 
            show . currentValue,
            show . bestBid . book,
            show . bestOffer . book,
            show . buySideLiquidity . book,
            show . sellSideLiquidity . book,
            show . buySideDepth . book,
            show . sellSideDepth . book] <*>) . return

  echoStates :: SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  echoStates result = do
    mapM_ (putStrLn . ("State: " ++) . show . output) . states $ result
    return result

  logStates :: FilePath -> SimResult MarketMessage Market -> IO (SimResult MarketMessage Market)
  logStates filename result = do
    let csv  = printCSV . map output . states $ result
    writeFile filename csv
    return result
  

