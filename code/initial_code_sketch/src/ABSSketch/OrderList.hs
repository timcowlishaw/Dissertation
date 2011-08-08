module ABSSketch.OrderList where
  import ABSSketch.Util
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

  instance Eq OrderList where
    (OrderList xs) == (OrderList ys) = and $ zipWith (\x y -> orders x == orders y && price x == price y) xs ys
  
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
  orderListDelete o ol@(OrderList os) = OrderList os''
    where os'' = fromMaybe os' $ (flip insert $ os') `fmap` justIf (not . null . orders) level'  
          os' = delete level os 
          level' = OrderListLevel (price level) (delete o $ orders level)
          level = levelForOrder ol o

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

  orderListPopLowest :: OrderList -> (Order, OrderList)
  orderListPopLowest ol@(OrderList ps) = (order, orderListDelete order ol)
    where order = last . orders . last $ ps 

  orderListFill :: OrderList -> Order -> OrderType -> [Trade] -> (OrderList, Order, [Trades])
  orderListFill ol@(OrderList []) order Buy trades = (ol, order, trades)
  orderListFill ol@(OrderList (p:ps)) order Buy trades | null . orders $ p              = orderListFill (OrderList ps) order Buy trades
                                                       | size order == 0                = (ol, order, trades)
                                                       | size order == size bookOrder   = (ol', order', trades')
                                                                                          where ol'     = orderListDelete bookOrder ol
                                                                                                order'  = order { size = 0}
                                                                                                trades' = Trade order bookOrder : trades
                                                       | size order < size bookOrder    = (ol', order', trades')
                                                                                          where ol'     = orderListInsert bookOrder' $ orderListDelete bookOrder ol
                                                                                                order'  = order { size = 0}
                                                                                                trades' = Trade order bookOrder'' : trades
                                                                                                bookOrder' = bookOrder  { size = size bookOrder' - size order}
                                                                                                bookOrder'' = bookOrder { size = size order } 
                                                       | otherwise                      = orderListFill (orderListDelete bookOrder ol) order Buy trades
                                                      where bookOrder = head . orders $ p   



 
   


