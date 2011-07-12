module ABSSketch where
  import ABSSketch.Timing
  import ABSSketch.LimitOrderBook
  --import ABSSketch.Traders
  --import ABSSketch.Sentiment
  import System.IO 

--  mainLoop :: Tick -> LimitOrderBook -> Handle -> IO ()
--  mainLoop t orderBook logFile = do
--    log orderBook
--    space
--    next
--    where log = (hPutStr logFile) . (++"\n")
--          space = hPutStr logFile "==================================\n"
--          next = case t of
--            5 -> return ()
--            _ -> mainLoop (t+1) (orderBook++"next iter! ") logFile
