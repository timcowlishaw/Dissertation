import Data.Map (Map)
import qualified Data.Map as M
import Data.List (transpose)
import Data.Aviary.Birds (thrush) -- thrush a f = f a

type SimArgs = Map String String
type SimState = Int
type Output = [SimState]
type SimMessage = Int
type SimAgent = SimState -> SimMessage

supervisor :: (SimArgs -> SimState) -> [SimAgent] -> (SimState -> [SimMessage] -> SimState) -> SimArgs -> Output
supervisor stateZero agents nextState ip = states
            where states = stateZero ip : (newState states messages)
                  messages = transpose $ map (thrush $ head states) agents
                  newState (s:ss) (m:ms) = nextState s m : newState ss ms

agent state = state + 1
initialState args = 0
combine state ms = state + sum ms

run = supervisor initialState [agent] combine M.empty
