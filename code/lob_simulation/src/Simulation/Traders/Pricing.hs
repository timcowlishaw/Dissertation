module Simulation.Traders.Pricing where
  import Simulation.Types
  import Simulation.Traders.Types
  import Simulation.Constants
  import Simulation.Market
  import Simulation.LimitOrderBook

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
          sensitivity  = volatilityFunction trader . fromIntegral $ buySideDepthNearTop (book market) topOfBookThreshold
      
 
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
          sensitivity = volatilityFunction trader . fromIntegral $ buySideDepthNearTop (book market) topOfBookThreshold
  

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
           sensitivity = volatilityFunction trader . fromIntegral $ sellSideDepthNearTop (book market) topOfBookThreshold

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
           sensitivity  = volatilityFunction trader . fromIntegral $  sellSideDepthNearTop (book market) topOfBookThreshold
