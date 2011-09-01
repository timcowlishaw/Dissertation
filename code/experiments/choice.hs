import Test.QuickCheck 
import Control.Applicative
import Control.Monad

newtype Choice x = Choice { unChoice :: [x] } deriving (Eq, Show)

instance Functor Choice where
  f `fmap` (Choice xs) = Choice (map f xs)

instance Applicative Choice where
  pure x = Choice [x]
  (Choice fs) <*> (Choice xs) = Choice . concat $ map (\x -> zipWith ($) fs (repeat x)) xs

instance Monad Choice where
  return            = pure
  (Choice xs) >>= f = Choice . concat . map (unChoice . f) $ xs

instance MonadPlus Choice where
  mzero                           = Choice []
  (Choice xs) `mplus` (Choice ys) = Choice (xs ++ ys) 
prop_functorPreservesIdentity :: Int -> Bool
prop_functorPreservesIdentity x = (id `fmap` f) == (id f)
                                   where f = pure x :: Choice Int

prop_functorPreservesComposition :: Int -> Bool
prop_functorPreservesComposition x = ((fmap h) . (fmap g)) f == (fmap (h . g) f)
                                      where f = pure x :: Choice Int
                                            g = (+5)
                                            h = (*3)


prop_monadLeftIdentity :: Int -> Bool
prop_monadLeftIdentity x = (return x >>= f) == (f x)
                              where f :: Int -> Choice Int
                                    f = return . (+5)  
prop_monadRightIdentity :: Int -> Bool
prop_monadRightIdentity x = (m >>= return) == m
                              where m :: Choice Int
                                    m = return x

prop_monadAssociativity :: Int -> Bool
prop_monadAssociativity x = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
                              where m :: Choice Int
                                    m = return x
                                    f :: Int -> Choice Int
                                    f = return . (+5)
                                    g :: Int -> Choice Int
                                    g = return . (*7)

prop_monoidAssociativity :: Int -> Int -> Int -> Bool
prop_monoidAssociativity x y z = ((m `mplus` n) `mplus` p) == (m `mplus` (n `mplus` p))
                                   where m = return x :: Choice Int
                                         n = return y :: Choice Int
                                         p = return z :: Choice Int


main = sequence $ quickCheck prop_monoidAssociativity : map quickCheck [prop_functorPreservesIdentity, prop_functorPreservesComposition, prop_monadLeftIdentity, prop_monadRightIdentity, prop_monadAssociativity] 
