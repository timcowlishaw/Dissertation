import Control.Monad

distM_ :: (Monad m) => a -> [a -> m b] -> m ()
distM_ input fs = foldl1 (\m n x -> m x >> n x) fs input >> return ()
