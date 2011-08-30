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

  data TraderSpec = TraderSpec String InventoryLevel InventoryLevel Int ImbalanceFunction VolatilityFunction Size

  type VolatilityFunction = Int -> Double
  type ImbalanceFunction = Int -> Double

  data Trader = Trader {
    traderID               ::  TraderID,
    initialInventory       ::  InventoryLevel,
    targetInventory        ::  InventoryLevel, 
    supplyDemand           ::  Int,
    --valueFunction          ::  ValueFunction,
    --inventoryFunction      ::  InventoryFunction, 
    imbalanceFunction      ::  ImbalanceFunction,
    priceChangeFunction    ::  VolatilityFunction,
    orderSizeLimit         ::  Size
  }
  
  topOfBookThreshold = 0.05

  intermediary :: TraderSpec
  intermediary = TraderSpec "intermediary" 0 1200 0 highImbalanceSensitivity lowVolatilitySensitivity 3000
  
  hfTrader :: TraderSpec
  hfTrader = TraderSpec "hfTrader" 0 800 0 highImbalanceSensitivity lowVolatilitySensitivity 3000

  fundamentalBuyer :: TraderSpec
  fundamentalBuyer = TraderSpec "fundamentalBuyer" 0 20000 200 lowImbalanceSensitivity highVolatilitySensitivity 3000
    
  fundamentalSeller :: TraderSpec
  fundamentalSeller = TraderSpec "fundamentalSeller" 0 100 (-200) lowImbalanceSensitivity highVolatilitySensitivity 3000

  opportunisticTrader :: TraderSpec
  opportunisticTrader = TraderSpec "opportunisticTrader" 0 800 0 mediumImbalanceSensitivity highVolatilitySensitivity 3000
  
  smallTrader :: TraderSpec
  smallTrader = TraderSpec "smallTrader" 0 400 0 mediumImbalanceSensitivity highVolatilitySensitivity 1

  lowImbalanceSensitivity :: ImbalanceFunction
  lowImbalanceSensitivity imbalance = 1
  
  mediumImbalanceSensitivity :: ImbalanceFunction
  mediumImbalanceSensitivity imbalance = (0.5-) . sigmoid . fromIntegral $ imbalance

  highImbalanceSensitivity :: ImbalanceFunction
  highImbalanceSensitivity imbalance = min 40 . exp . fromIntegral $ imbalance

  lowVolatilitySensitivity :: VolatilityFunction
  lowVolatilitySensitivity volatility = 1

  mediumVolatilitySensitivity :: VolatilityFunction
  mediumVolatilitySensitivity volatility = (0.5-) . sigmoid . fromIntegral $ volatility

  highVolatilitySensitivity :: VolatilityFunction
  highVolatilitySensitivity volatility = min 40 . exp . fromIntegral $ volatility   

  sigmoid x = 1 / (1 + exp(-x))

  makeTraders :: Int -> TraderSpec -> [Agent TraderState MarketMessage Market]
  makeTraders n (TraderSpec name ii ti sd is vs os) = map (\x -> makeAgent $ Trader (name ++ show x) ii ti sd is vs os) [1..n] 

  makeAgent :: Trader -> Agent TraderState MarketMessage Market
  makeAgent trader = Agent (traderID trader) function initialState 
    where initialState = TraderState $ initialInventory trader
          function market = do
            (TraderState inventoryLevel) <- (flip (!) $ traderID trader) `fmap` get
            let oSize           = orderSize trader market 
            let oType           = orderType oSize (orderSizeLimit trader)
            let pricingStrategy = if oSize > (orderSizeLimit trader) then aggressive else neutral
            let oPrice          = pricingStrategy oType trader market
            placeOrder oType oSize oPrice (traderID trader)
    
  placeOrder :: OrderTypeName -> Size -> Maybe Price -> TraderID -> SimState TraderState MarketMessage ()
  placeOrder t s p id = tell [makeOrder t s p id]
    where makeOrder Bid   oSize (Just oPrice) i = Message $ i `bid` oPrice `for` oSize
          makeOrder Offer oSize (Just oPrice) i = Message $ i `offer` oPrice `for` oSize
          makeOrder Buy   oSize _             i = Message $ i `buy` oSize
          makeOrder Sell  oSize _             i = Message $ i `sell` oSize

  outlook :: Market -> MarketOutlook
  outlook market | bb >= bo && bo >= cv = CrossedRising
                 | bb >= cv && cv >= bo = CrossedStable
                 | cv >= bb && bb >= bo = CrossedFalling
                 | bo >= bb && bb >= cv = Falling 
                 | bo >= cv && cv >= bb = Stable
                 | cv >= bo && bo >= bb = Rising
                 where bb = bestBid $ book market
                       bo = bestOffer $ book market
                       cv = currentValue market
 
  orderSize :: Trader -> Market -> Size
  orderSize trader market     = floor $ priceChangeF priceChange * orderImbalanceF imbalance
    where priceChangeF        = priceChangeFunction trader
          orderImbalanceF     = imbalanceFunction trader
          priceChange         = 2 * (bidMovement + offerMovement)
          imbalance           = ssDepth - bsDepth
          bsDepth             = buySideDepthNearTop book' topOfBookThreshold
          ssDepth             = sellSideDepthNearTop book' topOfBookThreshold
          bidMovement         = (bestBid book' - lastBestBid market) `div` lastBestBid market
          offerMovement       = (bestOffer book' - lastBestOffer market) `div` lastBestOffer market 
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
          sensitivity  = priceChangeFunction trader $ buySideDepthNearTop (book market) topOfBookThreshold
      
 
  aggressiveBuy :: Trader -> Market -> Price
  aggressiveBuy trader market
    | outlook' `elem` [CrossedFalling, CrossedStable, Rising] = bo
    | outlook' == CrossedRising                               = bb
    | outlook' == Falling                                     = (cv -) . floor $ abs (sensitivity * fromIntegral (bb - cv))
    | outlook' == Stable                                      = (bb +) . floor $ (sensitivity * fromIntegral (bb - cv))
    where outlook'    = outlook market
          bo          = bestOffer $ book market
          bb          = bestBid $ book market
          cv          = currentValue market
          sensitivity = priceChangeFunction trader $ buySideDepthNearTop (book market) topOfBookThreshold
  

  neutralSell :: Trader -> Market -> Price 
  neutralSell trader market
    | outlook' `elem` [CrossedFalling, CrossedStable, Falling] = bb
    | outlook' `elem` [Rising, CrossedRising]                  = cv
    | otherwise                                                = (bo -) . floor $ (sensitivity * fromIntegral sp)
    where  outlook'    = outlook market
           bo          = bestOffer $ book market
           bb          = bestBid $ book market
           cv          = currentValue market
           sp          = bo - bb
           sensitivity = priceChangeFunction trader $ sellSideDepthNearTop (book market) topOfBookThreshold

  aggressiveSell :: Trader -> Market -> Price   
  aggressiveSell trader market
    | outlook' == CrossedFalling                = bo
    | outlook' `elem` [CrossedStable, Falling]  = bb
    | outlook' == CrossedRising                 = cv
    | outlook' == Rising                        = (cv +) . floor $ abs (sensitivity * fromIntegral (cv - sp))
    | outlook' == Stable                        = (bo -) . floor $ (sensitivity * fromIntegral sp)
    where  outlook'     = outlook market
           bo           = bestOffer $ book market
           bb           = bestBid $ book market
           cv           = currentValue market
           sp           = bo - bb
           sensitivity  = priceChangeFunction trader $ sellSideDepthNearTop (book market) topOfBookThreshold
