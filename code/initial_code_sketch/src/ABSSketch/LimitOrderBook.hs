module ABSSketch.LimitOrderBook where
  import ABSSketch.Timing
  import ABSSketch.Orders
  import ABSSketch.Sentiment

  type Liquidity = Int

  data Trade = Trade {buy :: Order, sell :: Order}

  data LOB = LOB {
    bids :: OrderList,
    offers :: OrderList,
    buysWaiting :: [Order],
    sellsWaiting :: [Order],
    tradesDone :: [Trade],
    time :: Tick,
    ordersReceived :: [Order],
    sentiment :: Sentiment
    --traderInfo :: [TraderInfoItem]  
  }
 

  emptyLob :: LOB
  emptyLob = LOB emptyOrderList emptyΟrderList [] [] [] 0 [] Calm

  incrementTime :: LOB -> LOB
  incrementTime lob = lob {time = (time lob + 1)}

  getTime :: LOB -> Tick
  getTime = time
  
  getBestBid :: LOB -> Bid
  getBestBid = fst . bids
  
  getBestOffer :: LOB -> Offer
  getBestOffer = fst . offers

  getSentiment :: LOB -> Sentiment
  getSentiment = sentiment

  setSentiment :: LOB -> Sentiment -> LOB
  setSentiment lob s= lob{sentiment = s}
  
  getBuySideLiquidity :: LOB -> Liquidity
  getBuySideLiquidity = getLiquidity . bids

  getSellSideLiquidity :: LOB -> Liquidity
  getSellSideLiquidity = getLiquiduty . offers
  
  getMidPrice :: LOB -> Price
  getLastTradedPrice :: LOB -> Price
  
  getOrderNum :: LOB -> Num
  
  getBuySideLevels :: LOB -> Num
  getBuySideDepth :: LOB -> Num
  getBuySideDepthNearTop :: LOB -> Num
  
  getSellSideLevels :: LOB -> Num
  getSellSideDepth :: LOB -> Num
  getSellSideDepthNearTop :: LOB -> Num
  
  --getTraderInfo :: LOB -> TraderId -> TraderInfoItem 
  --setTraderInfo :: LOB -> TraderInfoItem -> LOB
  
  getTrace :: LOB -> String
  
  newBid   :: Price -> Order -> LOB -> LOB
  newOffer :: Price -> Order -> LOB -> LOB
  newTrade :: Price ->Order -> LOB -> LOB
  newOrder :: Order -> LOB -> LOB

  getTrades :: LOB -> [Trade]
  executeTrades :: LOB -> LOB

  is_crossed :: LOB -> Bool
  uncross :: LOB -> LOB

  match :: Order -> LOB -> LOB
  match_m1 :: Order -> LOB -> LOB 

  instance Show LOB where
    show lob = "Limit Order Book at time t="  ++
               (show $ time lob) ++
              "\n-----------------------------\n"++
              properties
                where properties = join "\n" . zipWith (\x f -> x ++": \n"++ (show . f $ lob)) $ nameMapping
                      nameMapping    = [("Offers", offers), ("Bids", bids), ("Buys waiting", buysWaiting), ("Sells waiting", sellsWaiting), ("Trades done", tradesDone), ("Orders received", ordersReceived), ("Sentiment", sentiment)]
