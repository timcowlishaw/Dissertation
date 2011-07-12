module ABSSketch.Test where
  import Test.QuickCheck
  import ABSSketch.Test.OrderList
  import Test.Framework (defaultMain, testGroup)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.QuickCheck
  
  main = defaultMain tests

  tests = [
            testGroup "OrderList" [
              testProperty "orderListFirst returns highest priced order" prop_orderListFirst_returnsHighestPricedOrder,
              testProperty "orderListLast returns lowest priced order" prop_orderListLast_returnsLowestPricedOrder,
              testProperty "isEmptyOrderList returns true when order list is empty" prop_isEmptyOrderList_returnsTrueIfEmpty,
              testProperty "isEmptyOrderList returns false when order list is not empty" prop_isEmptyOrderList_returnsFalseIfNotEmpty,
              testProperty "orderListContains returns false when list is empty" prop_orderListContains_returnsFalseIfEmpty,
              testProperty "orderListContains returns false when the list doesn't contain the passed order" prop_orderListContains_returnsFalseIfDoesntContainOrder,
              testProperty "orderListContains returns true when the list contains the passed order" prop_orderListContains_returnsTrueIfContainsOrder,
              testProperty "orderListInsert inserts Order at right level" prop_orderListInsert_insertsOrderAtRightLevel,
              testProperty "levelForOrder returns a level with the same price as the passed order" prop_levelForOrder_returnsPriceLevelForOrder,
              testProperty "levelForOrder returns a level including other orders at the same price" prop_levelForOrder_returnsLevelIncludingOtherOrdersOfSamePrice,
              testProperty "orderListDelete removes the specified order where it exists in the list" prop_orderListDelete_removesPassedOrder,
              testProperty "orderListDelete preserves other orders where it contains multiple orders and contains the passed order" prop_orderListDelete_returnsOrderListWithoutPassedOrderWithExistingOrdersWhenOrderExists,
              testProperty "orderListDelete preserves other orders in the list when it doesn't contain the passed order" prop_orderListDelete_returnsSameOrderListWhenOrderDoesntExist,
              testProperty "orderListDelete returns an empty order list when it is passed an empty order list" prop_orderListDelete_returnsEmptyOrderListWhenPassedEmptyOrderList,
              testProperty "orderListGetHighestPrice returns price of highest priced order" prop_orderListGetHighestPrice_returnsPriceOfHighestPricedOrder,
              testProperty "orderListGetLowestPrice returns price of lowest priced order" prop_orderListGetLowestPrice_returnsPriceOfLowestPricedOrder,
              testProperty "orderListGetDepth returns total size of orders at highest price when passed Offer as an order type" prop_orderListGetDepth_whenPassedOfferOrderType_returnsTotalSizeOfOrdersAtHighestPrice, 
              testProperty "orderListGetDepth returns total size of orders at lowest price when passed Bid as an order type" prop_orderListGetDepth_whenPassedBidOrderType_returnsTotalSizeOfOrdersAtLowestPrice
             ]
          ]
