module ABSSketch (mainLoop, emptyLob) where
  --import ABSSketch.LimitOrderBook
  --import ABSSketch.Traders
  --import ABSSketch.Sentiment
  import System.IO 

  type Tick = Int
  type LimitOrderBook = String

  emptyLob = ""

  mainLoop :: Tick -> LimitOrderBook -> Handle -> IO ()
  mainLoop t orderBook logFile = do
    log orderBook
    space
    next
    where log = (hPutStr logFile) . (++"\n")
          space = hPutStr logFile "==================================\n"
          next = case t of
            5 -> return ()
            _ -> mainLoop (t+1) (orderBook++"next iter! ") logFile
