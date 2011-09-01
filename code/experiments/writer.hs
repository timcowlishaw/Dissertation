import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans

type WriteyIntState = WriterT [Int] (StateT Int Identity) 
increment :: String -> WriteyIntState String
increment s = do
  let n = length s
  modify (+n)
  tell ([n])
  return (s ++ "x")

runWritey = map (runIdentity . flip runStateT 0 . runWriterT) . take 10 $ iterate (>>= increment) (return "")
