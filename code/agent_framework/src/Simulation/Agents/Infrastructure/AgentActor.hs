import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Actor.ActorBase
import Actor.ActorSyntax
import Actor.ActorCompiler
import Actor.ActorLinearSearch

fix f = f (fix f)
loop m = fix (m >>)

data Msg = Next | Nmbr (L Int) deriving (Eq, Show)

instance (Show a) => Show (L a) where
  show (Val x) = show x
  show (Var (VAR x)) = show x

msgHashOp = HashOp { numberOfTables = 2, hashMsg = \msg -> case msg of
                                                         Next -> 1
                                                         Nmbr _ -> 2 }
instance EMatch Msg where 
  match tags m1 m2 = return (m1 == m2, tags)

loopState :: Monad m => StateT s m a -> s -> m a
loopState m initState = evalStateT (loop m) initState

counter :: (Num s) => Act msg -> StateT s IO ()
counter other = do
  val <- get
  liftIO $ send (actorToPID other) (Nmbr val)  
  put (val + 1)



counterBehaviour :: Act Msg -> Act Msg -> IO ()
counterBehaviour printerActor self = do
  receive self [[Next] .->. (loopState $ counter printerActor) 0]

printer :: (Num s) => s -> IO ()
printer n = putStrLn $ "The number is" + show n

printerBehaviour :: Act Msg -> Act Msg -> IO ()
printerBehaviour counterActor self = loop (do
  x <- newVar :: IO (VAR Int)
  receive self [[Nmbr x] .->. (do
    x' <- readVar x 
    printer x'
    send (actorToPID counterActor) Next )]) 

main :: IO ()
main = do 
  pActor <- createActor msgHashOp
  cActor <- createActor msgHashOp
  runActor pActor (printerBehaviour cActor)
  runActor cActor (counterBehaviour pActor)
  send (actorToPID cActor) Next
