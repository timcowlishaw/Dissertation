module Simulation.Tests.LimitOrderList(limitOrderListTests) where
  import Test.HUnit
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Simulation.Order
  import Simulation.LimitOrderList

  limitOrderListTests = testGroup "Simulation.LimitOrderList" [
      testCase "bestPrice returns the highest bid price" testBestBid,
      testCase "bestPrice returns the lowest priced offer" testBestOffer,
      testCase "popBest returns the best order" testPopBestReturnsOrder,
      testCase "popBest returns the book without the best order" testPopBestReturnsBook
    ]

  testBestBid = do
    let orders = ["trader1" `bids` 200 `for` 100, "trader2" `bids` 500  `for` 200]
    let book = foldl insert empty orders
    bestPrice book @?= 500

  testBestOffer = do
    let orders = ["trader1" `offers` 500 `for` 100, "trader2" `offers` 300 `for` 50]
    let book = foldl insert empty orders
    bestPrice book @?= 300

  testPopBestReturnsOrder = do
    let best = "trader1" `bids` 500 `for` 100 
    let os = [best, "trader2" `bids` 400  `for` 200]
    let list = foldl insert empty os
    let (o, _) = popBest list
    o @?= best

  testPopBestReturnsBook = do
    let best = "trader1" `bids` 500 `for` 100 
    let os = [best, "trader2" `bids` 400  `for` 200]
    let list = foldl insert empty os
    let (_, b) = popBest list
    best `elem` (orders b) @?= False
