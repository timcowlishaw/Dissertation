module Simulation.Traders.Types where
  import Simulation.Types

  data HasInitialInventory = II Size
  data HasTargetInventory = TI Size
  data HasTargetOrderSize = TOS Size
  data HasDemandBias = DB Int
  data HasOrderSizeLimit = OSL Size
  data HasInventoryFunction = IF InventoryFunction

  data NoInitialInventory = NoII
  data NoTargetInventory = NoTI
  data NoTargetOrderSize = NoTOS
  data NoDemandBias = NoDB
  data NoOrderSizeLimit = NoOSL
  data NoInventoryFunction = NoIF

  class InitialInventory a
  instance InitialInventory HasInitialInventory
  instance InitialInventory NoInitialInventory

  class TargetInventory a
  instance TargetInventory HasTargetInventory
  instance TargetInventory NoTargetInventory

  class TargetOrderSize a
  instance TargetOrderSize HasTargetOrderSize
  instance TargetOrderSize NoTargetOrderSize

  class DemandBias a
  instance DemandBias HasDemandBias
  instance DemandBias NoDemandBias

  class OrderSizeLimit a
  instance OrderSizeLimit HasOrderSizeLimit
  instance OrderSizeLimit NoOrderSizeLimit

  class InventoryFunc a
  instance InventoryFunc HasInventoryFunction
  instance InventoryFunc NoInventoryFunction

  data (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf = Trader String ImbalanceFunction VolatilityFunction ii ti tos db osl inf

  type Trader = TraderType HasInitialInventory HasTargetInventory HasTargetOrderSize HasDemandBias HasOrderSizeLimit HasInventoryFunction

  data OrderTypeName = Buy | Sell | Bid | Offer deriving (Eq)
  type InventoryFunction  = Trader -> Price -> Price -> Size -> Size

  traderType :: String -> ImbalanceFunction -> VolatilityFunction -> TraderType NoInitialInventory NoTargetInventory NoTargetOrderSize NoDemandBias NoOrderSizeLimit NoInventoryFunction
  traderType name imbalance volatility = Trader name imbalance volatility NoII NoTI NoTOS NoDB NoOSL NoIF

  withInitialInventory :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> Size -> TraderType HasInitialInventory ti tos db osl inf
  withInitialInventory (Trader tid imbf vf _ ti tos db osl invf) ii = Trader tid imbf vf (II ii) ti tos db osl invf 

  withTargetInventory :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> Size ->  TraderType ii HasTargetInventory tos db osl inf
  withTargetInventory (Trader tid imbf vf ii _ tos db osl invf) ti = Trader tid imbf vf ii (TI ti) tos db osl invf 

  withTargetOrderSize :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> Size -> TraderType ii ti HasTargetOrderSize db osl inf
  withTargetOrderSize (Trader tid imbf vf ii ti _ db osl invf) tos = Trader tid imbf vf ii ti (TOS tos) db osl invf 

  withDemandBias :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> Int -> TraderType ii ti tos HasDemandBias osl inf
  withDemandBias (Trader tid imbf vf ii ti tos _ osl invf) db = Trader tid imbf vf ii ti tos (DB db) osl invf 

  withOrderSizeLimit :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> Size -> TraderType ii ti tos db HasOrderSizeLimit inf
  withOrderSizeLimit (Trader tid imbf vf ii ti tos db _ invf) osl = Trader tid imbf vf ii ti tos db (OSL osl) invf 

  withInventoryFunction :: (InitialInventory ii, TargetInventory ti, TargetOrderSize tos, DemandBias db, OrderSizeLimit osl, InventoryFunc inf) => TraderType ii ti tos db osl inf -> InventoryFunction -> TraderType ii ti tos db osl HasInventoryFunction
  withInventoryFunction (Trader tid imbf vf ii ti tos db osl _) invf = Trader tid imbf vf ii ti tos db osl (IF invf) 

  traderID :: Trader -> TraderID
  traderID (Trader tid _ _ _ _ _ _ _ _) = tid  
  
  imbalanceFunction :: Trader -> ImbalanceFunction
  imbalanceFunction (Trader _ imbf _ _ _ _ _ _ _) = imbf

  volatilityFunction ::  Trader -> VolatilityFunction
  volatilityFunction (Trader _ _ vf _ _ _ _ _ _) = vf  

  initialInventory :: Trader -> InventoryLevel
  initialInventory (Trader _ _ _ (II ii) _ _ _ _ _) = ii

  targetInventory :: Trader -> InventoryLevel 
  targetInventory (Trader _ _ _ _ (TI ti) _ _ _ _) = ti

  targetOrderSize :: Trader -> Size
  targetOrderSize (Trader _ _ _ _ _ (TOS tos) _ _ _) = tos

  supplyDemand :: Trader -> Int
  supplyDemand (Trader _ _ _ _ _ _ (DB db) _ _) = db

  orderSizeLimit :: Trader -> Size
  orderSizeLimit (Trader _ _ _ _ _ _ _ (OSL osl) _) = osl  

  inventoryFunction :: Trader -> InventoryFunction
  inventoryFunction (Trader _ _ _ _ _ _ _ _ (IF inf)) = inf

