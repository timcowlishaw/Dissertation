>|| This is a literate script - LIBRARY ROUTINES
>|| Orderlist, Order, Basic Types and Distributions
>|| Copyright Christopher D. Clack 2011


============
ORDERLIST

The bids and offers will be subject to the same ordering criterion (which we may wish to vary later) so make a generic abstype for inserting and deleting orders from the data structure

>abstype orderlist
>with
>    emptyorderlist :: orderlist
>    isemptyorderlist :: orderlist -> bool
>    ol_first :: orderlist -> (price,[order])
>    ol_last :: orderlist -> (price,[order])
>    ol_insert :: price -> orderlist -> order -> orderlist
>    ol_delete :: price -> orderlist -> order -> orderlist
>    ol_reverse :: orderlist -> orderlist
>    ol_gethighestprice :: orderlist -> price
>    ol_getlowestprice :: orderlist -> price
>    ol_getliquidity :: orderlist -> size
>    ol_getlevels :: orderlist -> num
>    ol_getdepth :: ordertype -> orderlist -> num
>    ol_getdepthneartop :: ordertype -> orderlist -> num
>    ol_poplowest :: orderlist -> (order, orderlist)
>    ol_clean :: orderlist -> orderlist
>    ol_fill :: orderlist -> order -> ordertype -> [(order, order)] -> (orderlist, order, [(order, order)])
>    ol_fill_cross :: orderlist -> order -> ordertype -> [(order, order)] -> (orderlist, order, [(order, order)])
>    showorderlist :: orderlist -> [char]

>|| implementation of orderlist
>orderlist == [(price, [order])]
>emptyorderlist = []
>
>ol_first [] = (0,[])
>ol_first (x:xs) = x
>
>ol_last [] = (0,[])
>ol_last [x] = x
>ol_last (x:xs) = ol_last xs

>isemptyorderlist [] = True
>isemptyorderlist any = False
>
>ol_insert p [] o = [(p,[o])]
>ol_insert p ((p,xs):ys) o 
>        = (p,newxs):ys
>          where
>          newxs = insert o xs
>          insert o [] = [o]
>          insert x (y:ys) 
>              = x:(y:ys), if (order_gettime x) < (order_gettime y) || timeorder
>              = y:(insert x ys), otherwise
>ol_insert p ((p2,xs):ys) o 
>        = ((p,[o]):((p2,xs):ys)), if p2 > p
>        = (p2,xs): (ol_insert p ys o), otherwise
>
>ol_delete p [] o = []
>ol_delete p ((p,xs):ys) o
>        = (p,newxs):ys
>          where
>          newxs = delete o xs
>          delete o [] = []
>          delete x (y:ys)
>              = ys, if (order_equals x y)
>              = y: (delete x ys), otherwise
>ol_delete p ((p2,xs):ys) o
>        = ((p2,xs):ys), if p2 > p
>        = (p2,xs):(ol_delete p ys o), otherwise
>
>ol_gethighestprice [] = error "Can't get highest price in empty list of orders"
>ol_gethighestprice [(p,xs)] = p
>ol_gethighestprice (x:xs) = ol_gethighestprice xs
>
>ol_getlowestprice [] = error "Can't get lowest price in empty list of orders"
>ol_getlowestprice ((p,xs):ys) = p
>
>ol_getliquidity [] = 0
>ol_getliquidity ((p,os):xs) = (foldr (+) 0 (map order_getsize os)) + (ol_getliquidity xs)
>
>ol_getlevels [] = 0
>ol_getlevels any = # any

>|| We define orderbook list depth as the sum of sizes of orders sitting at one price level at one end of the book
>|| We need to take an ordertype parameter to decide whether to look at the lowest price on the orderbook list (Offers)
>|| or the highest price o the orderbook list (Bids)
>ol_getdepth ty    []          = 0
>ol_getdepth Offer ((p,xs):ys) = sumsizes xs
>                                where
>                                sumsizes [] = 0
>                                sumsizes (o:os) = (order_getsize o) + (sumsizes os)
>ol_getdepth Bid   ((p,xs):[]) = sumsizes xs
>                                where
>                                sumsizes [] = 0
>                                sumsizes (o:os) = (order_getsize o) + (sumsizes os)
>ol_getdepth Bid   ((p,xs):ys) = ol_getdepth Bid ys

>|| We define orderbook list depth near top" as the sum of sizes of orders sitting at one price level at one end of the book
>|| plus sizes of other orders sitting on book with a price within 5% of the top price.
>|| We need to take an ordertype parameter to decide whether to look at the lowest price on the orderbook list (Offers)
>|| or the highest price o the orderbook list (Bids)
>ol_getdepthneartop ty    []          = 0
>ol_getdepthneartop Offer ((p,xs):ys) = (sumsizes xs) + (f p ys)
>                                       where
>                                       sumsizes [] = 0
>                                       sumsizes (o:os) = (order_getsize o) + (sumsizes os)
>                                       f p [] = 0
>                                       f p ((p1,xs):ys) = (sumsizes xs) + (f p ys), if (p1<(p*1.05))
>                                                        = 0, otherwise
>ol_getdepthneartop Bid   ys          = ol_getdepthneartop Offer (reverse ys)


>ol_poplowest [] = error "Can't pop lowest order from an empty list of orders"
>ol_poplowest ((p,[]):ys) = ol_poplowest ys
>ol_poplowest ((p,(x:xs)):ys) = (x, ((p,xs):ys))
>
>ol_clean  any = isort (filter (((~=[]).snd)) any) []
>                where
>                isort [] any = any
>                isort (x:xs) any = isort xs (insert x any)
>                insert x [] = [x]
>                insert (a,b) ((c,d):ys) = (a,b):((c,d):ys), if a<c
>                                        = (c,d):(insert (a,b) ys), otherwise
>
>
>||
>|| ol_fill takes a list of resting limit orders, a market order, a Buy/Sell indicator and a list of trades
>||     - The Buy/Sell indicator tells us in which direction to search the order list
>||     - The list of trades should start off as empty, since it is the list of fills to satisfy the market order
>|| ol_fill returns the remaining order list after removing the fills, the remaining market order if it wasn't competely filled,
>|| and the list of fills (the trades)
>||
>ol_fill []           m any tr = (emptyorderlist, m, tr)
>ol_fill ((p2,[]):ys) m Buy tr         ||easy - head of list is lowest Offer - try it first
>        = ol_fill ys m Buy tr
>ol_fill ((p2, (x:xs)):ys) m Buy  tr
>        = (((p2,(x:xs)):ys), emptyorder, tr), if (isemptyorder m)
>        = (((p2,xs):ys), emptyorder, newtr2), if msize = xsize
>        = (((p2,(newx:xs)):ys), emptyorder, newtr2), if msize < xsize
>        = ol_fill ((p2,xs):ys) newm Buy newtr, otherwise
>          where
>          xsize = order_getsize x
>          msize = order_getsize m
>          newm = order_newsize m (msize - xsize)
>          newtr = (x, (order_newprice (order_newsize m xsize) (order_getprice x))): tr
>          newx = order_newsize x (xsize - msize)
>          newtr2 = (order_newsize x msize, (order_newprice m (order_getprice x))):tr
>ol_fill os m Sell tr = ol_fill (ol_reverse os) m Buy tr 
>ol_fill os m any tr = error "Cannot fill order - market order not provided"

> ||
> || ol_fill_cross is used for a corssed book.  In this case the market order isn't really a market order - it is
> || an offer limit order that is crossed with a bid limit order.  We therefore execute at the mean price.
> || Also, we have to keep checking after each execution whether the book is still crossed or not - we no longer
> || have access to the book, so we check whether (mprice > xprice) which would mean no longer crossed.
>
>ol_fill_cross []           m any tr = (emptyorderlist, m, tr)
>ol_fill_cross ((p2,[]):ys) m Buy tr         ||easy - head of list is lowest Offer - try it first
>        = ol_fill_cross ys m Buy tr
>ol_fill_cross ((p2, (x:xs)):ys) m Buy  tr
>        = (((p2,(x:xs)):ys), m, tr), if (mprice > xprice)
>        = (((p2,(x:xs)):ys), emptyorder, tr), if (isemptyorder m)
>        = (((p2,xs):ys), emptyorder, newtr2), if msize = xsize
>        = (((p2,(newx:xs)):ys), emptyorder, newtr2), if msize < xsize
>        = ol_fill_cross ((p2,xs):ys) newm Buy newtr, otherwise
>          where
>          xsize = order_getsize x
>          msize = order_getsize m
>          xprice = order_getprice x
>          mprice = order_getprice m
>          executeprice = (mprice + xprice)/2
>          newm = order_newsize m (msize - xsize)
>          newtr = (order_newprice x executeprice, (order_newprice (order_newsize m xsize) executeprice)): tr
>          newx = order_newsize x (xsize - msize)
>          newtr2 = (order_newprice (order_newsize x msize) executeprice, (order_newprice m executeprice)):tr
>ol_fill_cross os m Sell tr = ol_fill_cross (ol_reverse os) m Buy tr 
>ol_fill_cross os m any tr = error "Cannot fill crossed order - offer not provided"


>ol_reverse [] = []
>ol_reverse (x:xs) = (ol_reverse xs) ++ [x]
>
>showorderlist [] = "ENDOL"
>showorderlist ((p,xs):ys) 
>    = (show p) ++ ":   " ++ (show xs) ++ "\n" ++ (showorderlist ys)

============

============
ORDER
We might need to change the details of what's contained in an order, so here's an abstype for an order

>abstype order
>with
>    emptyorder :: order
>    isemptyorder :: order -> bool
>    order_create :: price -> size -> time -> traderid -> ordertype -> order
>    order_getprice :: order -> price
>    order_setprice :: price -> order -> order
>    order_gettime :: order -> time
>    order_gettype :: order -> ordertype
>    order_gettraderid :: order -> traderid
>    order_getsize :: order -> size
>    order_newsize :: order -> size -> order
>    order_newprice :: order -> price -> order
>    order_equals :: order -> order -> bool
>    showorder :: order -> [char]

>|| implementation of order
>order == (price, size, time, traderid, ordertype)
>emptyorder = (0, 0, 0, Phantom, None)
>isemptyorder (0, 0, 0, x, any) = True
>isemptyorder any = False
>order_create p s t tid ty = (p, s, t, tid, ty)
>order_getprice (p, s, t, tid, ty) = p
>order_setprice newp (p, s, t, tid, ty) = (newp, s, t, tid, ty)
>order_gettime (p, s, t, tid, ty) = t
>order_getsize (p, s, t, tid, ty) = s
>order_gettype (p, s, t, tid, ty) = ty
>order_gettraderid (p, s, t, tid, ty) = tid
>order_newsize (p, s, t, tid, ty) news  = (p, news, t, tid, ty), if (news >= 0)
>                                       = error ("Updating order to negative size " ++ (show news)), otherwise
>order_newprice (p, s, t, tid, ty) newp = (newp, s, t, tid, ty), if (newp >= 0)
>                                       = error ("Updating order to negative price " ++ (show newp)), otherwise
>order_equals (p, s, t, tid, ty) (p, s, t, tid, ty) = True
>order_equals x y = False
>
>showorder (p, s, t, tid, ty) 
>    = "($" ++ (show p) ++ ", " ++ (show s) ++ " shares, at " ++ (show t) ++ " secs,  from " ++ (show tid) ++ ", of type " ++ (show ty) ++ ")\n     "

============

============
BASIC TYPES

>price == num
>size == num
>time == num
>ordertype ::= Bid | Offer | Sell | Buy | None | Abort
>traderid ::= Phantom | Intermediary num | HFT num |  FundSeller num | FundBuyer num |  Small num | Opportunistic num |
>             SSTaker num | BSTaker num | BSMaker num | SSMaker num | Maker num | Taker num
>sentiment ::= Calm | Choppy | Ramp | Toxic

============

===============================
DISTRIBUTIONS

>|| rands generates random numbers in the range 0 - 1,000
>rands :: num -> [num]
>rands seed = x : (rands x)
>             where
>             x = ((entier (((abs seed) * 1103515245 + 12345)/65536)) mod 32768) mod 1000

>|| gaussians generates a list of gaussian numbers in the range -6 to 6
>|| Most of the numbers will therefore be in the range -1 to 1
>gaussians :: num -> [num]
>gaussians seed = x : (gaussians x)
>                 where
>                 r x = ((rands seed) ! x)/ 1000
>                 randoms = map r [1..12]
>                 sum_of_rands = foldr (+) 0 randoms
>                 x = sum_of_rands - 6

>|| value is a function that gives the underlying (fundamental) value of the stock being traded as a function of time.  It can be uniform
>|| (the easiest case) or rising, falling or sinusoidal.
>
> value Calm   t = 2000
> value Choppy t = ((sines ! t) + 1.0  ) * 1000 + 1000
> value Ramp   t = ([0.5,0.501 ..]!t) * 1000 + 1000
> value Toxic  t = 2000, if t < 450
>                = 5, otherwise

>sines:: [num]
>sines = [sin x | x <- [0.0,0.01 .. (2*pi)]] ++ sines
>||sines = [sin x | x <- [6.28,6.27 .. 0]] ++ sines

>num_interm = 3
>num_hft = 3
>num_fundbuyer = 2
>num_fundseller = 2
>num_small = 4
>num_opp = 4
>num_sst = 0
>num_bst = 0
>num_bsm = 0
>num_ssm = 0


==============================


