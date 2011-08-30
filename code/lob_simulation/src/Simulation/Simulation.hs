{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simulation.Simulation where
  import Control.Applicative
  import Control.Arrow
  import Control.Monad
  import Control.Monad.State
  import Control.Monad.Writer
  import Data.Map (Map)
  import qualified Data.Map as M

  type AgentID = String
 
  newtype SimState a m s = SimState { unState :: StateT (Map AgentID a) (WriterT [m] IO) s } deriving (Monad, MonadIO, MonadWriter [m], MonadState (Map AgentID a), Functor)

  data Agent a m s = Agent { agentID :: AgentID,
                             behaviour :: s -> SimState a m (),
                             initialAgentState :: a
                           }  

  data Simulation a m s = Simulation { agents :: [Agent a m s], 
                                       initialState :: s,
                                       reduce :: s -> [m] -> SimState a m s
                                     } 
  
  data SimResult m s = SimResult {states :: [s], log :: [m]}

  runSim :: Int -> Simulation a m s -> IO (SimResult m s) 
  runSim iterations sim = makeResult (simStep sim) (initialState sim)
    where makeResult step init = liftM ((uncurry SimResult) . (take iterations *** take iterations)) . runWriterT . flip evalStateT agentStates . unState $ states
            where agentStates = M.fromList . map (agentID &&& initialAgentState) . agents $ sim
                  states      = iterateM step init
 
  simStep :: Simulation a m s -> s -> SimState a m s
  simStep sim state = do
    let functions = map behaviour $ agents sim
    (_, messages) <- listen $ return state >>= distM_ functions
    (reduce sim) state messages 
 

  distM_ :: (Monad m) => [a -> m b] -> a -> m ()
  distM_ fs x = sequence_ . map ($x) $ fs

  iterateM :: Monad m => (a -> m a) -> a -> m [a]
  iterateM f a = (a:) `liftM` (f a >>= iterateM f)

  defaultMain :: IO (SimResult m s) -> IO ()
  defaultMain x = return ()   
