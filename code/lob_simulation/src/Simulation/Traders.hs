module Simulation.Traders (Trader, makeTraders) where
  import Simulation.Types
  import Simulation.Traders.Types
  import Simulation.Traders.Pricing
  import Simulation.Utils
  import Simulation.Constants
  import Simulation.Order hiding (Market, traderID) 
  import Simulation.Market
  import Simulation.LimitOrderBook hiding (placeOrder)
  import Simulation.Trade
  import Simulation.OrderResponse
  import Simulation.Simulation
  import Data.Map (Map, (!))
  import Control.Monad.Writer
  import Control.Monad.State
    
  makeTraders :: [(Int,Trader)] -> [Agent TraderState MarketMessage Market]
  makeTraders pairs = pairs >>= uncurry fromPair
    where fromPair n trader = map (flip makeAgent trader) [1..n] 

  makeAgent :: Int -> Trader -> Agent TraderState MarketMessage Market
  makeAgent n trader = Agent tid function initialState
    where tid = (show n ++ traderID trader)
          initialState = TraderState $ initialInventory trader
          function market = do
            (TraderState inventoryLevel) <- (flip (!) $ tid) `fmap` get
            let targetInv       = inventoryLevel + supplyDemand trader * time market
            let oSize           = orderSize trader market targetInv 
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
  orderSize trader market target                 = floor $ priceChangeF priceChange * orderImbalanceF imbalance * fromIntegral (inventoryFunction trader trader (currentValue market) (midPrice . book  $ market) target)
    where priceChangeF                           = volatilityFunction trader
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
 
