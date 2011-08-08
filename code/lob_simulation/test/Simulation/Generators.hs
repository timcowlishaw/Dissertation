{-# LANGUAGE FlexibleInstances #-}
module Simulation.Generators where
  import Test.QuickCheck
  import Simulation.Order
  import Simulation.MultiLevelList

  instance Arbitrary (Order Buy Limit) where
    arbitrary = do
      price <- choose(100, 20000)
      size <- choose (1, 200)
      return $ bid price `for` size

  instance Arbitrary (Order Sell Limit) where
    arbitrary = do
      price <- choose (100, 20000)
      size <- choose (1, 200)
      return $ ask price `for` size

  instance Arbitrary (MultiLevelList (Order Buy Limit)) where
    arbitrary = do
      numOrders <- choose (1,20)
      orders <- sequence $ replicate numOrders arbitrary
      return $ foldl (flip insert) empty orders
