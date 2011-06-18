import ABSSketch
import System.IO
import System.Exit
import System.Environment

main :: IO ()
main = do 
  [filename] <- getArgs
  file <- openFile filename WriteMode
  mainLoop 0 emptyLob file
  hClose file
  exitSuccess

