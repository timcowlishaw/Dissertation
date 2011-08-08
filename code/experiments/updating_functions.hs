import Control.Monad.State

data Inc = Inc { runInc :: (Int -> (Int, Inc)) }

-- f(0) = 1 
-- s(0) = 0
-- f(n+1) = f(n) + s(n)
-- s(n+1) = s(n) + 1
 
incrementing_add :: Int -> Inc

incrementing_add state = Inc $ \n -> (state+n, incrementing_add (state+1))

triangles = map fst $ iterate (\(val, inc) -> runInc inc $ val ) (0, incrementing_add 1)

incrementingAddM n = do
  s <- get
  put $ s + 1
  return $ n + s

triangles' = map fst $ iterate (f incrementingAddM) (0,1)
  where f func = \(st, num) -> (runState $ (return st) >>= func) num 


data Agent state input = {
  function :: (MonadState state m) => input -> m input,
  initialState :: state }



