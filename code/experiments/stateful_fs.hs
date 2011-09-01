data Agent s x = Agent {function :: (x -> State s [Message]), initialState :: s}

take n = modify (+n) >> return (Subtract n)
give n = (guard (>0) . (-n) $ get) >> modify (-n) >> return (Add n)


type Message = Add Int | Subtract Int

reduce :: x -> [Message] -> x
reduce x ms = (foldr f x ms)
  where f n (Add n')      = n + n'
        f n (Subtract n') = n - n'

takerWhenEven = Agent (takerWhenEvenF 500) 0

takerWhenEvenF limit n = do
  guard (even n)
  inv <- get
  take $ min n ((limit - inv) `div` 4)

giverWhenOddF reserve n = do
  guard (odd f)
  inv <- get
  give $ (inv - reserve) `div` 5

giverWhenOdd = Agent (giverWhenOddF 50) 250

agents = [takerWhenEven, giverWhenOdd]

nums = runSim' 30 agents reducedata Inc = Inc { f :: (Int -> (Int, Inc)) }
 
runSim' :: x -> [Agent s x] -> ([Message] -> x) -> [x]
runSim' = map fst . runState (sequence . iterate (>>= ) $) (map initialStates agents)



thrush a f = f a
appliedToAll x = map (thrush x)

newtype ParallelApply x = ParA [x]

--AGENTS ARE ARROWS IN THE KLEISLI CATEGORY OF THE SIMULATION MONAD.

instance Monad ParallelApply x where
  return x = ParA $ replicate x
  
