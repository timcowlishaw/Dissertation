module Simulation.Traders.InventoryFunctions(intermediaryInventory, hfInventory, fundamentalBuyerInventory, fundamentalSellerInventory, opportunisticTraderInventory, smallTraderInventory) where
  
  import Simulation.Types
  import Simulation.Utils
  import Simulation.Traders.Types 
  intermediaryInventory :: InventoryFunction
  
  intermediaryInventory trader value price inventory = bounded (-2000) amount 2000
    where amount = floor $ 0.2 * (-(tan (y/700)))* fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory
  
  hfInventory :: InventoryFunction
  hfInventory trader value price inventory = bounded (-2000) amount 2000
    where amount = floor $ 0.2* (-(tan (y/700)))* fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory
  
  fundamentalBuyerInventory :: InventoryFunction
  fundamentalBuyerInventory trader value price inventory = bounded (-2000) amount 2000
    where amount = floor $ exp (y/150) - fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory

  fundamentalSellerInventory :: InventoryFunction
  fundamentalSellerInventory trader value price inventory = bounded (-2000) amount 2000
    where amount = floor $ exp (-y/150) - fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory

  opportunisticTraderInventory :: InventoryFunction
  opportunisticTraderInventory trader value price inventory = bounded (-250) amount 250
    where amount = floor $ (- tan (y/700)) * fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory
  
  smallTraderInventory :: InventoryFunction
  smallTraderInventory trader value price inventory = bounded (-250) amount 250
    where amount = floor$ (- tan (y/700)) * fromIntegral (targetOrderSize trader)
          y      = inventoryWeight trader value price inventory
    
  inventoryWeight trader value price inventory = fromIntegral inventory - fromIntegral (targetInventory trader) * ((1+) . sigmoid . fromIntegral $ value - price)
