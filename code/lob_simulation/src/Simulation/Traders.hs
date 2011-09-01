module Simulation.Traders (Trader, makeTraders, makeAgent, intermediary, hfTrader, fundamentalBuyer, fundamentalSeller, opportunisticTrader, smallTrader) where
  import Simulation.Types
  import Simulation.Order hiding (Market, traderID) 
  import Simulation.Market
  import Simulation.LimitOrderBook hiding (placeOrder)
  import Simulation.Trade
  import Simulation.OrderResponse
  import Simulation.Simulation
  import Data.Map (Map, (!))
  import Control.Monad.Writer
  import Control.Monad.State
 

  data OrderTypeName = Buy | Sell | Bid | Offer deriving (Eq)

  data TraderSpec = TraderSpec String InventoryLevel InventoryLevel Int InventoryFunction ImbalanceFunction VolatilityFunction Size

  type VolatilityFunction = Double -> Double
  type ImbalanceFunction = Int -> Double

  data Trader = Trader {
    traderID               ::  TraderID,
    initialInventory       ::  InventoryLevel,
    targetInventory        ::  InventoryLevel, 
    targetOrderSize        ::  Size
    supplyDemand           ::  Int,
    --valueFunction          ::  ValueFunction,
    inventoryFunction      ::  InventoryFunction, 
    imbalanceFunction      ::  ImbalanceFunction,
    priceChangeFunction    ::  VolatilityFunction,
    orderSizeLimit         ::  Size
  }
  
  topOfBookThreshold = 0.05

  intermediary :: TraderSpec
  intermediary = TraderSpec "intermediary" 0 1200 200 0 intermediaryInventory highImbalanceSensitivity lowVolatilitySensitivity 3000
  
  hfTrader :: TraderSpec
  hfTrader = TraderSpec "hfTrader" 0 800 100 0 hfInventory highImbalanceSensitivity lowVolatilitySensitivity 3000

  fundamentalBuyer :: TraderSpec
  fundamentalBuyer = TraderSpec "fundamentalBuyer" 0 20000 200 200 fundamentalBuyerInventory lowImbalanceSensitivity highVolatilitySensitivity 3000
    
  fundamentalSeller :: TraderSpec
  fundamentalSeller = TraderSpec "fundamentalSeller" 0 100 200 (-200) fundamentalSellerInventory lowImbalanceSensitivity highVolatilitySensitivity 3000

  opportunisticTrader :: TraderSpec
  opportunisticTrader = TraderSpec "opportunisticTrader" 0 800 100 0 opportunisticTraderInventory mediumImbalanceSensitivity highVolatilitySensitivity 3000
  
  smallTrader :: TraderSpec
  smallTrader = TraderSpec "smallTrader" 0 400 100 0 smallTraderInventory mediumImbalanceSensitivity highVolatilitySensitivity 1

  intermediaryInventory trader value price inventory = bounded (-2000) amount 2000
    where amount = 0.2 * (-(tangent (y/700)))* (targetInventory trader) 
          y      = inventory -(target)

  hfInventory
  fundamentalBuyerInventory
  fundamentalSellerInventory
  opportunisticTraderInventory
  smallTraderInventory

  bounded = (min .) . max

  lowImbalanceSensitivity :: ImbalanceFunction
  lowImbalanceSensitivity = const 1
  
  mediumImbalanceSensitivity :: ImbalanceFunction
  mediumImbalanceSensitivity = (0.5-) . sigmoid . fromIntegral

  highImbalanceSensitivity :: ImbalanceFunction
  highImbalanceSensitivity = min 40 . exp . fromIntegral

  lowVolatilitySensitivity :: VolatilityFunction
  lowVolatilitySensitivity = const 1

  mediumVolatilitySensitivity :: VolatilityFunction
  mediumVolatilitySensitivity = (0.5-) . sigmoid

  highVolatilitySensitivity :: VolatilityFunction
  highVolatilitySensitivity = min 40 . exp   

  sigmoid x = 1 / (1 + exp(-x))

  makeTraders :: Int -> TraderSpec -> [Agent TraderState MarketMessage Market]
  makeTraders n (TraderSpec name ii ti sd is vs os) = map (\x -> makeAgent $ Trader (name ++ show x) ii ti sd is vs os) [1..n] 

  makeAgent :: Trader -> Agent TraderState MarketMessage Market
  makeAgent trader = Agent (traderID trader) function initialState 
    where initialState = TraderState $ initialInventory trader
          function market = do
            (TraderState inventoryLevel) <- (flip (!) $ traderID trader) `fmap` get
            let targetInventory = inventoryLevel + supplyDemand * time
            let oSize           = orderSize trader market targetInventory 
            let oType           = orderType oSize (orderSizeLimit trader)
            let pricingStrategy = if oSize > (orderSizeLimit trader) then aggressive else neutral
            let oPrice          = pricingStrategy oType trader market
            placeOrder (time market) oType (abs oSize) oPrice (traderID trader)
    
  placeOrder :: Time -> OrderTypeName -> Size -> Maybe Price -> TraderID -> SimState TraderState MarketMessage ()
  placeOrder time t s p id = tell [makeOrder t s p id]
    where makeOrder Bid   oSize (Just oPrice) id = Message time $ id `bids` oPrice `for` oSize
          makeOrder Offer oSize (Just oPrice) id = Message time $ id `offers` oPrice `for` oSize
          makeOrder Buy   oSize _             id = Message time $ id `buys` oSize
          makeOrder Sell  oSize _             id = Message time $ id `sells` oSize
 
  orderSize :: Trader -> Market -> Size -> Size
  orderSize trader market target                 = floor $ priceChangeF priceChange * orderImbalanceF imbalance * inventoryFunction trader (currentValue market) (midPrice market) target
    where priceChangeF                           = priceChangeFunction trader
          orderImbalanceF                        = imbalanceFunction trader
          priceChange                            = 2 * (bidMovement + offerMovement)
          imbalance                              = ssDepth - bsDepth
          bsDepth                                = buySideDepthNearTop book' topOfBookThreshold
          ssDepth                                = sellSideDepthNearTop book' topOfBookThreshold
          bidMovement   | lastBestBid market > 0 = fromIntegral (bestBid book' - lastBestBid market) / fromIntegral (lastBestBid market)
                        | otherwise =  0.05
          offerMovement | lastBestOffer market > 0 = fromIntegral (bestOffer book' - lastBestOffer market) / fromIntegral (lastBestOffer market)
                        | otherwise = 0.05
          book'               = book market 
 
  orderType :: Size -> Size -> OrderTypeName 
  orderType orderSize sizeLimit | orderSize < 0 && abs orderSize  >= sizeLimit  = Sell
                                | orderSize < 0                                 = Offer
                                | orderSize >= 0 && abs orderSize >= sizeLimit  = Buy
                                | otherwise                                     = Bid
  
  aggressive :: OrderTypeName -> Trader -> Market -> Maybe Price
  aggressive orderType trader market = case orderType of
    Bid   -> Just $ aggressiveBuy trader market
    Offer -> Just $ aggressiveSell trader market
    Buy   -> Nothing
    Sell  -> Nothing
                
  neutral :: OrderTypeName -> Trader -> Market -> Maybe Price
  neutral orderType trader market = case orderType of
    Bid   -> Just $ neutralBuy trader market
    Offer -> Just $ neutralSell trader market
    Buy   -> Nothing
    Sell  -> Nothing
              

  neutralBuy :: Trader -> Market -> Price
  neutralBuy trader market
    | outlook' `elem` [CrossedStable, CrossedRising, Rising] = bo
    | outlook' == Stable                                     = floor $ fromIntegral bb + 0.5 * sensitivity
    | otherwise                                              = cv
    where outlook'     = outlook market
          bo           = bestOffer $ book market
          bb           = bestBid $ book market
          cv           = currentValue market
          sensitivity  = priceChangeFunction trader . fromIntegral $ buySideDepthNearTop (book market) topOfBookThreshold
      
 
  aggressiveBuy :: Trader -> Market -> Price
  aggressiveBuy trader market
    | outlook' `elem` [CrossedFalling, CrossedStable, Rising] = bo
    | outlook' == CrossedRising                               = bb
    | outlook' == Falling                                     = floor . (fromIntegral cv -) . abs $ sensitivity * fromIntegral (bb - cv)
    | outlook' == Stable                                      = floor . (fromIntegral bb +) $ sensitivity * fromIntegral (bb - cv)
    where outlook'    = outlook market
          bo          = bestOffer $ book market
          bb          = bestBid $ book market
          cv          = currentValue market
          sensitivity = priceChangeFunction trader . fromIntegral $ buySideDepthNearTop (book market) topOfBookThreshold
  

  neutralSell :: Trader -> Market -> Price 
  neutralSell trader market
    | outlook' `elem` [CrossedFalling, CrossedStable, Falling] = bb
    | outlook' `elem` [Rising, CrossedRising]                  = cv
    | otherwise                                                = abs . (bo -) . floor $ (sensitivity * fromIntegral sp)
    where  outlook'    = outlook market
           bo          = bestOffer $ book market
           bb          = bestBid $ book market
           cv          = currentValue market
           sp          = bo - bb
           sensitivity = priceChangeFunction trader . fromIntegral $ sellSideDepthNearTop (book market) topOfBookThreshold

  aggressiveSell :: Trader -> Market -> Price   
  aggressiveSell trader market
    | outlook' == CrossedFalling                = bo
    | outlook' `elem` [CrossedStable, Falling]  = bb
    | outlook' == CrossedRising                 = cv
    | outlook' == Rising                        = abs . (cv +) . floor $ abs (sensitivity * fromIntegral (cv - sp))
    | outlook' == Stable                        = abs . (bo -) . floor $ (sensitivity * fromIntegral sp)
    where  outlook'     = outlook market
           bo           = bestOffer $ book market
           bb           = bestBid $ book market
           cv           = currentValue market
           sp           = bo - bb
           sensitivity  = priceChangeFunction trader . fromIntegral $  sellSideDepthNearTop (book market) topOfBookThreshold
