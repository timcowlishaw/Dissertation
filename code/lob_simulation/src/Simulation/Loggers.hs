module Simulation.Loggers(Loggable,  runLoggers, echoStates, echoMessages, logStates, logMessages, none, output) where
  import Simulation.Utils
  import Simulation.Simulation
  import Simulation.LimitOrderBook
  import Simulation.Trade
  import Simulation.Order hiding (Market)
  import Simulation.OrderResponse
  import Simulation.Market
  import Text.CSV
  import Control.Monad
  import Control.Arrow
  import Data.List
  import System.Directory
  import Control.Monad.Error
   
  class Loggable a where
    output :: a -> [(Field, Field)]

  data (Loggable s, Loggable m) => Logger s m = Logger {
    setup :: s -> [m] -> IO (),
    run :: s -> [m] -> IO ()
  } 

  none = ""

  keys :: (Loggable a) => a -> Record
  keys = map fst . output

  values :: Loggable a => a -> Record
  values = map snd . output

  runLoggers :: (Loggable s, Loggable m, Message m) => [Logger s m] -> SimResult m s -> IO (SimResult m s)
  runLoggers loggers result = do
    let pairs = zip (states result) (groupBy sameTime . messages $ result)
    distM_ (map (uncurry . setup) loggers) $ head pairs
    mapM_ (distM_ $ map (uncurry . run) loggers) $ pairs
    return result

  echoStates :: (Loggable s, Loggable m) => Logger s m
  echoStates = Logger noSetup runEchoStates
  
  echoMessages :: (Loggable s, Loggable m) => Logger s m
  echoMessages = Logger noSetup runEchoMessages

  logStates :: (Loggable s, Loggable m) => FilePath -> Logger s m
  logStates path = Logger (setupLogStates path) (runLogStates path)

  logMessages :: (Loggable s, Loggable m) => FilePath -> Logger s m
  logMessages path = Logger (setupLogMessages path) (runLogMessages path)
  

  runEchoStates :: (Loggable s, Loggable m) =>  s -> [m] -> IO ()
  runEchoStates state messages = do
    putStrLn . ("State: " ++) . show . values $ state

  runEchoMessages :: (Loggable s, Loggable m) =>  s -> [m] -> IO ()
  runEchoMessages state messages = do
    mapM_ (putStrLn . ("    Message: " ++) . show . values) $ messages

  runLogStates :: (Loggable s, Loggable m) => FilePath -> s -> [m] -> IO ()
  runLogStates filename state messages = do
    let csv  = printCSV . return . values $ state
    appendFile filename $ csv ++ "\n"
  
  runLogMessages :: (Loggable s, Loggable m) => FilePath -> s -> [m] -> IO ()
  runLogMessages filename state messages = do
    let csv = printCSV . map values $ messages
    appendFile filename csv


  setupLogStates :: (Loggable s, Loggable m) => FilePath -> s -> [m] -> IO ()
  setupLogStates  filename state messages = do
    exists <- doesFileExist filename
    if exists then
      removeFile filename
      else return ()
    appendFile filename $ printCSV (return . keys $ state) ++ "\n"
    return ()
  setupLogMessages :: (Loggable s, Loggable m) => FilePath -> s -> [m] -> IO ()
  setupLogMessages filename state messages = do
    exists <- doesFileExist filename
    if exists then
      removeFile filename
      else return ()
    appendFile filename $ printCSV (return . keys . head $ messages) ++ "\n"
  noSetup :: (Loggable s, Loggable m) => s -> [m] -> IO ()
  noSetup state messages = return ()

  sameTime :: (Message m1, Message m2) => m1 -> m2 -> Bool
  sameTime m1 m2 = messageTime m1 == messageTime m2 
