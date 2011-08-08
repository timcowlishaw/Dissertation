--type Generator = [Int]
--type Value = Double
--generator = [1..]
--
--data St =  St {gen :: Generator, val :: Value} deriving (Show)
--
--data Agent = Agent { f :: State Int Int, initState :: Int}
--
--thrush a f = f a
--
--initState = St generator 0.0
--
--actorF (St (g:gs) val) = ((-1)^g)/(2*fromIntegral g+1) 
--
--reducer state vals = St (drop (length vals) (gen state)) (val state + sum vals)
--
--actor = Agent actorF 0
--
--actors = [actor, actor, actor, actor]
--
--runSim state0 agents reduce = iterate (\st -> reduce st . flip map agents . thrush $ st) state0

data (Monoid m) => SimState m =  SimState m

instance Monoid (SimState m) where
  mzero   (SimState m)              = mzero m
  mappend (SimState m) (SimState n) = mappend m n 

data Sim m  = Sim { runSim :: [SimState m] }

instance Functor (Sim m) where
  f `fmap` (Sim sl@(s:ss)) = (f s):sl

instance Monad (Sim m) where
  return s              = Sim [s]
  (Sim sl@(s:ss)) >>= f =  
