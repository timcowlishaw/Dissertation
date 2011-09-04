module Simulation.Utils where
  import Control.Monad

  bounded :: (Ord a) => a -> a -> a -> a
  bounded = (min .) . max  
 
  sigmoid x = 1 / (1 + exp(-x))

  distM_ :: (Monad m) => [a -> m b] -> a -> m ()
  distM_ fs x = sequence_ . map ($x) $ fs

  iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
  iterateM (-1)  _ _ = return []
  iterateM  n    f a = (a:) `liftM` (f a >>= iterateM (n-1) f) 
