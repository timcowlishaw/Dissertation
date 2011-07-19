module ABSSketch.OrderList where
  import ABSSketch.Order hiding (price)
  import qualified ABSSketch.Order as O (price)
  import Data.List
  import Data.Maybe
  import Data.List.Ordered
  import Control.Monad
   
  data OrderListLevel = OrderListLevel {
    price :: Price,
    orders :: [Order]
  } deriving (Show)


  --TODO - Order should be a typeclass with implementations for bid and offer with their own Ord instances, getting rid of the need to flip the order list for Bids. Should Ord be a requirement for Order? not sure.
  instance Ord OrderListLevel where
    compare x y = compare (price y) (price x) -- inverted as highest first
  
  instance Eq OrderListLevel where
    x == y = (price x) == (price y)

  newtype OrderList = OrderList [OrderListLevel]
  
  --TODO: remove orderList prefix to method name and use idiomatic list operations with import qualified as where there are clashes in a certain namespace.

  emptyOrderList :: OrderList
  emptyOrderList = OrderList []

  orderListFirst :: OrderList -> Order
  orderListFirst (OrderList xs) = head . orders . head $ xs

  orderListLast :: OrderList -> Order
  orderListLast (OrderList xs) = head . orders . last $ xs

  isEmptyOrderList :: OrderList -> Bool
  isEmptyOrderList (OrderList xs) = all (null . orders) xs

  orderListContains :: OrderList -> Order -> Bool
  orderListContains ol o = o `elem` (orders $ levelForOrder ol o)

  orderListInsert :: Order -> OrderList -> OrderList
  orderListInsert o ol@(OrderList os) = OrderList . insert level . delete level' $ os
    where level = OrderListLevel (price level') (o:orders level')
          level' = levelForOrder ol o

  levelForOrder :: OrderList -> Order -> OrderListLevel
  levelForOrder (OrderList os) o = fromMaybe (OrderListLevel p []) $ find ((== p) . price) os 
    where p = O.price o


  orderListDelete ::  Order -> OrderList -> OrderList
  orderListDelete o ol@(OrderList os) = OrderList $ insert level os    
    where level = OrderListLevel (price level') (delete o $ orders level')
          level' = levelForOrder ol o

  orderListGetHighestPrice :: OrderList -> Price
  orderListGetHighestPrice (OrderList (p:ps)) = price p

  orderListGetLowestPrice :: OrderList -> Price
  orderListGetLowestPrice (OrderList ps) = (price . last) ps
 
  orderListGetLevels :: OrderList -> Int
  orderListGetLevels (OrderList ps) = length ps
  
  orderListGetDepth :: OrderType -> OrderList -> Int
  orderListGetDepth _ (OrderList []) = 0
  orderListGetDepth Offer (OrderList (p:ps)) = sum $ map size (orders p)
  orderListGetDepth Bid (OrderList ps) = sum $ map size (orders $ last ps)

  orderListGetDepthNearTop :: OrderType -> OrderList -> Int
  orderListGetDepthNearTop Bid ol@(OrderList ps) = sum . map (sum . map size . orders) $ ps'
    where ps' = filter ((>= (floor $ 0.95* fromIntegral max)) . fromIntegral . price) ps
          max = orderListGetHighestPrice ol

  orderListGetDepthNearTop Offer ol@(OrderList ps) = sum . map (sum . map size . orders) $ ps
    where ps' = filter ((<= (floor $ 1.05* fromIntegral min)) . fromIntegral . price) ps
          min = orderListGetLowestPrice ol 




 
   


