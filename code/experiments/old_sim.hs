module Simulation.Simulation where

  import Control.Monad.Trans.Writer
  import Control.Monad.Trans.Reader 
  import Data.List (transpose)
  import Data.Map (Map)
  import qualified Data.Map as M
  
  newtype SimLogItem    a   = SimLogItem { logValue :: a }
  newtype SimMessage    a   = SimMessage { message :: a }
 
  type SimConfig  k v        = Data.Map k v
  type SimLog         l      = [SimLogItem l]
  type SimState   k v l s    = ReaderT (SimConfig k v) WriterT (SimLog l) Identity s
  type SimAgent         s m  = s -> [SimMessage m]
  type SimReducer     l s m  = SimState l s -> [[SimMessage m]] -> SimState l s

  log :: a -> Writer (SimLog a) ()
  log x = tell . (:[]) . SimLogItem $ x
 
  thrush x f = f x -- Smullyan's Thrush combinator, from 'To mock a mockingbird'

  runSim :: SimState l s -> [SimAgent l s m] -> SimReducer l s m -> [SimState l s]
  runSim state0 agents reduce = states
                                  where states                  = state0 : nextState states messages
                                        messages                = transpose . map (flip map agents . thrush) $ states
                                        nextState (s:ss) (m:ms) = reduce s m : nextState ss ms
 

  runSim' state0 agents reduce = iterate (\st -> reduce st . flip map agents . thrush $ st) state0
