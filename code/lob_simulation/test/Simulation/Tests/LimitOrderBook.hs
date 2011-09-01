module Simulation.Tests.LimitOrderBook(limitOrderBookTests) where
  import Test.HUnit
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Simulation.Order
  import Simulation.LimitOrderBook

  limitOrderBookTests = testGroup "Simulation.LimitOrderBook" [
      testCase "isCrossed returns true when the best bid is higher priced than the best order" testCrossedTrue,
      testCase "isCrossed returns false when the best bid is lower priced than the best order" testCrossedFalse
    ]

  testCrossedTrue = do
    let book = empty `withTrades` ["trader1" `bids` 5000 `for` 70] `withTrades` [ "trader2" `offers` 70 `for` 30]
    isCrossed book @?= True


  testCrossedFalse = do
    let book = empty `withTrades` ["trader1" `bids` 30 `for` 70] `withTrades` ["trader2" `offers` 5000 `for` 30]
    isCrossed book @?= False
