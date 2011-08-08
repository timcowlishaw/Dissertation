import Control.Concurrent.CHP
import qualified Control.Concurrent.CHP.Common as CHP
import Control.Concurrent.CHP.Console
import Control.Concurrent.CHP.Connect
import Control.Monad
import Control.Monad.Trans
import System.Random
import Data.Char

main ::IO ()
main = runCHP_ (consoleProcess printOrdWhileWaiting)

echo :: ConsoleChans -> CHP()
echo chans = CHP.id (cStdin chans) (cStdout chans)

not_x :: ConsoleChans -> CHP ()
not_x chans = CHP.filter (/= 'x') (cStdin chans) (cStdout chans)

printOrd :: ConsoleChans -> CHP ()
printOrd chans = forever (do
  x <- readChannel (cStdin chans)
  let ordXStr = show (fromEnum x) ++ "\n"
  mapM_ (writeChannel (cStdout chans)) ordXStr
  )

printOrdWhileWaiting :: ConsoleChans -> CHP ()
printOrdWhileWaiting chans = readChannel cin >>= inner
  where
    inner :: Char -> CHP ()
    inner x = do
      printString (show (fromEnum x) ++ "\n")
      (waitFor 1000000 >> inner x) <-> (readChannel cin >>= inner)
    cin = cStdin chans
    cout = cStdout chans
    cerr = cStderr chans
    printString s = mapM_ (writeChannel cout) s <||> mapM_ (writeChannel cerr) s

crazyPipeline :: ConsoleChans -> CHP ()
crazyPipeline chans = do
  pipelineConnect [CHP.filter isLetter, CHP.map toUpper, CHP.filter (/= 'X')] (cStdin chans) (cStdout chans)
  return ()

spaghettiFork :: Chanin () -> Chanin () -> CHP ()
spaghettiFork claimA claimB = forever $ (readChannel claimA >> readChannel claimA) <-> (readChannel claimB >> readChannel claimB)

philosopher :: Chanout () -> Chanout () -> CHP ()

philosopher left right = forever (do
  randomDelay -- Thinking
  writeChannel left () >> writeChannel right ()
  randomDelay -- Eating
  writeChannel left () <||> writeChannel right ()
  )
  where
    randomDelay = liftIO_CHP (getStdRandom (randomR (0,50000))) >>= waitFor

college :: CHP ()
college = do
  phil0Left <- newChannel' $ chanLabel "phil0Left"
  phil0Right <- newChannel' $ chanLabel "phil0Right"
  phil1Left <- newChannel' $ chanLabel "phil1Left"
  phil1Right <- newChannel' $ chanLabel "philRight"
  runParallel_ 
    [ philosopher (writer phil0Left) (writer phil0Right),
      philosopher (writer phil1Left) (writer phil1Right),
      spaghettiFork (reader phil0Left) (reader phil1Right),
      spaghettiFork (reader phil1Left) (reader phil0Right)]

