module ABSSketch.Test.Generators where
  import Test.QuickCheck
  import ABSSketch.Order
  import ABSSketch.Traders
  
  arbitraryBid :: Gen Order
  arbitraryBid = do
    price <- choose (1, 100)
    size <- choose (1, 200)
    time <- choose (1, 10000)
    return $ Order price size time Trader Bid

  
  arbitraryOffer :: Gen Order
  arbitraryOffer = do
    price <- choose (1, 100)
    size <- choose (1, 200)
    time <- choose (1, 10000)
    return $ Order price size time Trader Offer

  instance Arbitrary Order where
    arbitrary = oneof [arbitraryBid, arbitraryOffer]


