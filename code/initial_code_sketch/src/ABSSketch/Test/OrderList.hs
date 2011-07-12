module ABSSketch.Test.OrderList where
  import Test.QuickCheck
  import Test.QuickCheck.Property
  import ABSSketch.Order hiding (price)
  import qualified ABSSketch.Order as O (price)
  import ABSSketch.OrderList
  import ABSSketch.Test.Generators
  

  prop_orderListFirst_returnsHighestPricedOrder highOrder lowOrder = (O.price lowOrder < O.price highOrder) ==> highOrder == orderListFirst orderList
    where orderList = foldl (flip orderListInsert) emptyOrderList [lowOrder, highOrder]
  
  prop_orderListLast_returnsLowestPricedOrder highOrder lowOrder = (O.price lowOrder < O.price highOrder) ==> lowOrder == orderListLast orderList
    where orderList = foldl (flip orderListInsert) emptyOrderList [lowOrder, highOrder]

  prop_isEmptyOrderList_returnsTrueIfEmpty = isEmptyOrderList emptyOrderList

  prop_isEmptyOrderList_returnsFalseIfNotEmpty order = not $ isEmptyOrderList (orderListInsert order emptyOrderList)


  prop_orderListContains_returnsFalseIfEmpty order = not $ orderListContains emptyOrderList order

  prop_orderListContains_returnsFalseIfDoesntContainOrder order1 order2 = not $ orderListContains (orderListInsert order1 emptyOrderList) order2

  prop_orderListContains_returnsTrueIfContainsOrder order = orderListContains (orderListInsert order emptyOrderList) order
 
  prop_orderListInsert_insertsOrderAtRightLevel order = (price $ levelForOrder orderlist order) == (O.price order)
    where orderlist = orderListInsert order emptyOrderList

  prop_levelForOrder_returnsPriceLevelForOrder order = (price $ levelForOrder emptyOrderList order) == O.price order

  prop_levelForOrder_returnsLevelIncludingOtherOrdersOfSamePrice order1 order2 = (O.price order1 == O.price order2) ==> (orders $ levelForOrder orderList order2) == [order1]
    where orderList = orderListInsert order1 emptyOrderList
  
  prop_orderListDelete_removesPassedOrder order = not $ orderListContains (orderListDelete order orderList) order
    where orderList = orderListInsert order emptyOrderList

  prop_orderListDelete_returnsOrderListWithoutPassedOrderWithExistingOrdersWhenOrderExists order1 order2 = orderListContains (orderListDelete order1 orderList) order2
    where orderList = foldl (flip orderListInsert) emptyOrderList [order1, order2]

  prop_orderListDelete_returnsSameOrderListWhenOrderDoesntExist order1 order2 = orderListContains (orderListDelete order1 orderList) order2
    where orderList = orderListInsert order2 emptyOrderList

  prop_orderListDelete_returnsEmptyOrderListWhenPassedEmptyOrderList order1 = isEmptyOrderList $ orderListDelete order1 emptyOrderList

  prop_orderListGetHighestPrice_returnsPriceOfHighestPricedOrder order1 order2 = max (O.price order1) (O.price order2) == orderListGetHighestPrice orderList
    where orderList = foldl (flip orderListInsert) emptyOrderList [order1, order2]

  prop_orderListGetLowestPrice_returnsPriceOfLowestPricedOrder order1 order2 = min (O.price order1) (O.price order2) == orderListGetLowestPrice orderList
    where orderList = foldl (flip orderListInsert) emptyOrderList [order1, order2]

  prop_orderListGetDepth_whenPassedOfferOrderType_returnsTotalSizeOfOrdersAtHighestPrice :: Price -> Price -> Order -> Property
  prop_orderListGetDepth_whenPassedOfferOrderType_returnsTotalSizeOfOrdersAtHighestPrice price1 price2 order = forAll (sized $ \n -> choose (1 , n)) $ \ numHighest ->
    (forAll (sized $ \n -> choose (1,n)) $ \numLowest -> 
      (let  lowestPrice = min price1 price2
            highestPrice = max price1 price2
            lowOrders = take numLowest $ repeat (order {O.price = lowestPrice})
            highOrders = take numHighest $ repeat (order {O.price = highestPrice})
            orderList = foldl (flip orderListInsert) emptyOrderList (lowOrders ++ highOrders) 
       in (price1 /= price2) ==> (orderListGetDepth Offer orderList == (sum $ map size highOrders))))     

  prop_orderListGetDepth_whenPassedBidOrderType_returnsTotalSizeOfOrdersAtLowestPrice :: Price -> Price -> Order -> Property
  prop_orderListGetDepth_whenPassedBidOrderType_returnsTotalSizeOfOrdersAtLowestPrice price1 price2 order = forAll (sized $ \n -> choose (1 , n)) $ \ numHighest ->
    (forAll (sized $ \n -> choose (1,n)) $ \numLowest -> 
      (let  lowestPrice = min price1 price2
            highestPrice = max price1 price2
            lowOrders = take numLowest $ repeat (order {O.price = lowestPrice})
            highOrders = take numHighest $ repeat (order {O.price = highestPrice})
            orderList = foldl (flip orderListInsert) emptyOrderList (lowOrders ++ highOrders) 
       in (price1 /= price2) ==> (orderListGetDepth Bid orderList == (sum $ map size lowOrders))))      
