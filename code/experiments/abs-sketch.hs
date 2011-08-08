import Data.Monoid
import Control.Applicative 

newtype SimState s = SimState s
type Agent s = SimState s -> SimState s
data Sim s = Sim  {agents :: [Agent s], initialState :: SimState s}

instance Functor SimState where
  f `fmap` (SimState s) = SimState $ f s

instance Applicative SimState where
  pure a = SimState a
  (SimState f) <*> (SimState s) = SimState $ f s

instance (Monoid s) => Monoid (SimState s) where
  mempty        = pure mempty
  mappend s1 s2 = mappend <$> s1 <*> s2


runSim :: (Monoid s) => Sim s -> [SimState s]

runSim (Sim agents initState) = iterate (\st -> mconcat . map (thrush st) $ agents)  initState


--helper functions:

thrush a f = f a
