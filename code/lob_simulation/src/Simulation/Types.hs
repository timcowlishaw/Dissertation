module Simulation.Types where
  type Price = Int
  type Size = Int
  type Time = Int
  type TraderID = String
  type Penalty = Int
  type InventoryLevel = Int
  
  type VolatilityFunction = Double -> Double
  type ImbalanceFunction  = Int -> Double

