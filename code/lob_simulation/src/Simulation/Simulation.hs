{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Simulation where
  import Simulation.Utils
  import Control.Applicative
  import Control.Arrow
  import Control.Monad
  import Control.Monad.State
  import Control.Monad.Writer
  import Data.Map (Map)
  import qualified Data.Map as M

  type AgentID = String

  class Message m where
    messageTime :: m -> Int
 
  newtype (Message m) => SimState a m s = SimState { unState :: StateT (Map AgentID a) (WriterT [m] IO) s } deriving (Monad, MonadIO, MonadWriter [m], MonadState (Map AgentID a), Functor)

  data (Message m) => Agent a m s = Agent { agentID :: AgentID,
                             behaviour :: s -> SimState a m (),
                             initialAgentState :: a
                           }  

  data (Message m) => Simulation a m s = Simulation { agents :: [Agent a m s], 
                                       initialState :: s,
                                       reduce :: s -> [m] -> SimState a m s
                                     } 
  
  data (Message m) => SimResult m s = SimResult {states :: [s], messages :: [m]}

  runSim :: (Message m) => Int -> Simulation a m s -> IO (SimResult m s) 
  runSim iterations sim = makeResult (simStep sim) (initialState sim)
    where makeResult step init  = liftM (uncurry SimResult) . runWriterT . flip evalStateT agentStates . unState $ states
            where agentStates           = M.fromList . map (agentID &&& initialAgentState) . agents $ sim
                  states                = iterateM iterations step init
 
  simStep :: (Message m) => Simulation a m s -> s -> SimState a m s
  simStep sim state = do
    let functions = map behaviour $ agents sim
    (_, messages) <- listen $ return state >>= distM_ functions
    (reduce sim) state messages 
 
