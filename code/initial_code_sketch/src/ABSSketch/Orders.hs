module ABSSketch.Orders where
  import ABSSketch.Timing  
  import ABSSketch.Traders
  import Data.List.Ordered
  import Control.Monad
   
  type Price = Int

  data OrderType = Bid | Offer | Sell | Buy | None | Abort deriving (Eq, Show) 
  
  data Order = Order {
    price :: Price,
    size :: Int,
    time :: Tick,
    trader :: Trader,
    orderType :: OrderType
  } deriving (Eq, Show)

  data OrderListLevel = OrderListLevel {
    price :: Price,
    orders :: [Order]
  } deriving (Show)

  instance Ord OrderListLevel where
    compare = compare.price


  newtype OrderList = OrderList [OrderListLevel]
  
  --TODO: remove orderList prefix to method name and use idiomatic list operations with import qualified as where there are clashes in a certain namespace.

  emptyOrderList :: OrderList
  emptyOrderList = OrderList []

  orderListFirst :: OrderList -> Order
  orderListFirst (OrderList xs) = fst . orders . fst $ xs

  orderListLast :: OrderList -> Order
  orderListLast (OrderList xs) = fst . orders . last $ xs

  isEmptyOrderList :: OrderList -> Bool
  isEmptyOrderList (OrderList xs) = isEmpty xs

  orderListInsert :: Order -> OrderList -> OrderList
  orderListInsert o ol@(OrderList os) = OrderList . insert level os $ insert o level
    where level = levelForOrder ol o 

  levelForOrder :: OrderList -> Order -> OrderListLevel
  levelForOrder (OrderList os) o = fromMaybe (OrderList p []) $ find ((== p) . price) os 
    where p = price o


  orderListDelete ::  Order -> OrderList -> OrderList
  orderListDelete o ol@(OrderList os) = OrderList . insert level os $ delete o level
    where level = levelForOrder ol o


