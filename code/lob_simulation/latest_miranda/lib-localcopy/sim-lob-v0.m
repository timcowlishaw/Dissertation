>|| This is a literate script - LIBRARY ROUTINES
>|| Limit Order Book
>|| Copyright Christopher D. Clack 2011
>%include "../lib/sim-orderlist.m" 

====================
LIMIT ORDER BOOK

>abstype lob
>with
>    emptylob :: lob
>    primed_emptylob :: lob
>    lob_increment_time :: lob -> lob 
>    lob_gettime :: lob -> time
>    lob_getbestbid :: lob -> price
>    lob_getbestoffer :: lob -> price
>    lob_getsentiment :: lob -> sentiment
>    lob_setsentiment :: sentiment -> lob -> lob
>    lob_getbuysideliquidity :: lob -> size
>    lob_getsellsideliquidity :: lob -> size
>    lob_getmidprice :: lob -> price
>    lob_getlasttradedprice :: lob -> price
>    lob_getordernum :: lob -> num
>    lob_getbuysidelevels :: lob -> num
>    lob_getbuysidedepth :: lob -> num
>    lob_getbuysidedepthneartop :: lob -> num
>    lob_getsellsidelevels :: lob -> num
>    lob_getsellsidedepth :: lob -> num
>    lob_getsellsidedepthneartop :: lob -> num
>    lob_gettraderinfo :: traderid -> lob -> traderinfo_item
>    lob_settraderinfo :: traderinfo_item -> lob -> lob
>    lob_gettrace :: lob -> [char]
>    lob_getstats :: lob -> statstype
>    newbid :: price -> order -> lob -> lob
>    newoffer :: price -> order -> lob -> lob
>    newtrade :: price -> order -> lob -> lob
>    neworder :: order -> lob -> lob
>    lob_gettrades :: lob -> trades_done
>    execute_trades :: lob -> lob
>    is_crossed_book :: lob -> bool
>    uncross_book :: lob -> lob
>    match :: order -> lob -> lob
>    match_m1 :: order -> lob -> lob
>    showlob :: lob -> [char]
>

>|| Implementation of lob
>lob == underlyinglob
>underlyinglob ::= LOB bids offers buys_waiting sells_waiting trades_done time orders_received sentiment traderinfo statstype
>lobtrace == [[char]]
>bids == orderlist
>offers == orderlist
>buys_waiting == [order]
>sells_waiting == [order]
>trades_done == [(order, order)]
>orders_received == [order]
>traderinfo == [traderinfo_item]
>traderinfo_item == (traderid, percent_liquidity_taken, penalty_applied, time)   || time is time penalty was applied
>percent_liquidity_taken == num
>penalty_applied == num
>statstype == (price,price,num,num,         ||  underlying value, last traded price, squared error, cumulative squared error,
>              (num,num,num,num),           ||  minliquiditybs, maxliquiditybs, liquiditybs, maxdrawdownbs,
>              (num,num,num,num),           ||  minliquidityss, maxliquidityss, liquidityss, maxdrawdownss
>              (num,num,num,num),           ||  minliquiditybstop, maxliquiditybstop, liquiditybstop, maxdrawdownbstop,
>              (num,num,num,num),           ||  minliquiditysstop, maxliquiditysstop, liquiditysstop, maxdrawdownsstop
>              (num,num,num,num),           ||  zeroliquiditybs, maxzeroliquiditybs, zeroliquidityss, maxzeroliquidityss
>              (num,num,num,num)            ||  zeroliquiditybstop, maxzeroliquiditybstop, zeroliquiditysstop, maxzeroliquiditysstop
>             )
>
>|| We add two orders into the empty order book to "seed" it for stability (otherwise we have to wait several ticks for the book to stabililze)
>emptylob = LOB emptyorderlist emptyorderlist 
>||               (ol_insert ((value Calm 0)*0.9) emptyorderlist (order_create ((value Calm 0)*0.9) 500 0 Phantom Bid))
>||               (ol_insert ((value Calm 0)*1.1) emptyorderlist (order_create ((value Calm 0)*1.1) 500 0 Phantom Offer))
>               [] [] [] 0 [] Calm [] (0,0,0,0,(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0))

>primed_emptylob = match (order_create 2050 500 0 Phantom Offer) (match (order_create 1950 500 0 Phantom Bid) emptylob)

>emptytraderinfo_item = (Phantom, 0, 0, 0)

>lob_getstats (LOB b o bw sw tr ti or sen tinfo stats)
>    = stats

>lob_increment_time (LOB b o bw sw tr ti or sen tinfo (uv, ltp, squerr, cumsquerr, 
>                                                          (minlbs,maxlbs,lbs,maxddbs), 
>                                                          (minlss,maxlss,lss,maxddss),
>                                                          (minlbstop,maxlbstop,lbstop,maxddbstop), 
>                                                          (minlsstop,maxlsstop,lsstop,maxddsstop),
>                                                          (zerolbs, maxzerolbs, zerolss, maxzerolss),
>                                                          (zerolbstop, maxzerolbstop, zerolsstop, maxzerolsstop)
>                                                     )) 
>    = LOB b o bw sw tr (ti+1) or sen tinfo newstats 
>      where
>      newuv = value sen (ti+1)
>      newltp = 0, if (tr=[])
>             = order_getprice (fst (hd tr)), otherwise
>      newsquerr = (newltp - newuv)^2
>      newcumsquerr = cumsquerr + newsquerr
>      || max drawdown buyside total liquidity :-
>      newlbs = (ol_getliquidity b) 
>      newmaxlbs = max [maxlbs, newlbs]
>      ddbs = maxlbs - min [ newlbs, minlbs]
>      newmaxddbs = max [maxddbs, ddbs]
>      newminlbs = min [newlbs, minlbs], if (maxlbs = newmaxlbs)
>                = newmaxlbs, otherwise
>      || max drawdown sellside total liquidity :-
>      newlss = (ol_getliquidity o) 
>      newmaxlss = max [maxlss, newlss]
>      ddss = maxlss - min [ newlss, minlss]
>      newmaxddss = max [maxddss, ddss]
>      newminlss = min [newlss, minlss], if (maxlss = newmaxlss)
>                = newmaxlss, otherwise
>      || max drawdown buyside top 5% of book :-
>      newlbstop = (ol_getdepthneartop Bid b) 
>      newmaxlbstop = max [maxlbstop, newlbstop]
>      ddbstop = maxlbstop - min [ newlbstop, minlbstop]
>      newmaxddbstop = max [maxddbstop, ddbstop]
>      newminlbstop = min [newlbstop, minlbstop], if (maxlbstop = newmaxlbstop)
>                   = newmaxlbstop, otherwise
>      || max drawdown sellside top 5% of book :-
>      newlsstop = (ol_getdepthneartop Offer b) 
>      newmaxlsstop = max [maxlsstop, newlsstop]
>      ddsstop = maxlsstop - min [ newlsstop, minlsstop]
>      newmaxddsstop = max [maxddsstop, ddsstop]
>      newminlsstop = min [newlsstop, minlsstop], if (maxlsstop = newmaxlsstop)
>                   = newmaxlsstop, otherwise
>      newzerolbs       = zerolbs + 1, if newlbs = 0
>                       = 0,otherwise
>      newmaxzerolbs    = maxzerolbs, if newlbs = 0
>                       = max[maxzerolbs, zerolbs], otherwise
>      newzerolss       = zerolss + 1, if newlss = 0
>                       = 0,otherwise
>      newmaxzerolss    = maxzerolss, if newlss = 0
>                       = max[maxzerolss, zerolss], otherwise
>      newzerolbstop    = zerolbstop + 1, if newlbstop = 0
>                       = 0,otherwise
>      newmaxzerolbstop = maxzerolbstop, if newlbstop = 0
>                       = max [maxzerolbstop, zerolbstop], otherwise
>      newzerolsstop    = zerolsstop + 1, if newlsstop = 0
>                       = 0,otherwise
>      newmaxzerolsstop = maxzerolsstop, if newlsstop = 0
>                       = max [maxzerolsstop, zerolsstop], otherwise
>      newstats = (newuv, newltp, newsquerr, newcumsquerr, 
>                                                          (newminlbs,newmaxlbs,newlbs,newmaxddbs), 
>                                                          (newminlss,newmaxlss,newlss,newmaxddss),
>                                                          (newminlbstop,newmaxlbstop,newlbstop,newmaxddbstop), 
>                                                          (newminlsstop,newmaxlsstop,newlsstop,newmaxddsstop),
>                                                          (newzerolbs, newmaxzerolbs, newzerolss, newmaxzerolss),
>                                                          (newzerolbstop, newmaxzerolbstop, newzerolsstop, newmaxzerolsstop)
>                 )

>lob_gettrace ob
>    = (show ti) ++ "," ++			                         || Time
>      (show (lob_getbuysideliquidity ob)) ++ "," ++                     || Buy Side Liquidity
>      (show (lob_getsellsideliquidity ob)) ++ "," ++                    || Sell Side Liquidity
>      ||   (show (lob_getmidprice ob)) ++ "," ++                        || Mid Price
>      (show (lob_getbestbid ob)) ++ "," ++                              || Best Bid
>      (show (lob_getbestoffer ob)) ++ "," ++                            || Best Offer 
>      (show (ltp)) ++ "," ++                                            || Last Traded Price
>      (show (# (lob_gettrades ob)))  ++ "," ++                          || Cumulative Trades
>      (show (lob_getbuysidelevels ob))  ++ "," ++                       || Buy Side Levels 
>      (show (lob_getsellsidelevels ob))  ++","++                        || Sell Side Levels
>      (show (lob_getbuysidedepth ob))  ++","++                          || Buy Side Depth
>      (show (lob_getsellsidedepth ob))  ++","++                         || Sell Side Depth
>      (show uv) ++ "," ++                                               || Current value
>      (show (lob_getbuysidedepthneartop ob))  ++","++                   || Buy Side Depth Near Top
>      (show (lob_getsellsidedepthneartop ob))  ++","++                  || Sell Side Depth Near Top
>      (show rms)  ++ "," ++                                             || Cumulative Root Mean Squared error LTP-UV
>      (show maxdrawdownbs)  ++ "," ++                                   || Maximum drawdown of buyside liquidity
>      (show maxdrawdownss)  ++ "," ++                                   || Maximum drawdown of sellside liquidity
>      (show maxdrawdownbstop)  ++ "," ++                                || Maximum drawdown of buyside liquidity top 5% of book
>      (show maxdrawdownsstop)  ++ "," ++                                || Maximum drawdown of sellside liquidity top 5% of book
>      (show maxzerolbs)  ++ "," ++                                      || Maximum zero buyside liquidity
>      (show maxzerolss)  ++ "," ++                                      || Maximum zero sellside liquidity
>      (show maxzerolbstop)  ++ "," ++                                   || Maximum zero buyside liquidity top 5% of book
>      (show maxzerolsstop)  ++                                          || Maximum zero sellside liquidity top 5% of book
>       "\n"
>      where
>      ti = lob_gettime ob
>      (uv, ltp, squerr, cumsquerr, bsdrawdown, ssdrawdown, bsdrawdowntop, ssdrawdowntop, zeros, zerostop) = lob_getstats ob
>      (b1, b2, b3, maxdrawdownbs) = bsdrawdown
>      (s1, s2, s3, maxdrawdownss) = ssdrawdown
>      (b1top, b2top, b3top, maxdrawdownbstop) = bsdrawdowntop
>      (s1top, s2top, s3top, maxdrawdownsstop) = ssdrawdowntop
>      (z1, maxzerolbs, z3, maxzerolss) = zeros
>      (z1top, maxzerolbstop, z3top, maxzerolsstop) = zeros
>      msquerr = cumsquerr / ti, if (ti > 0)
>              = cumsquerr, otherwise
>      rms     = sqrt msquerr

>lob_gettime (LOB b o bw sw tr ti or sen tinfo stats) = ti
>lob_getbestbid (LOB b o bw sw tr ti or sen tinfo stats) 
>        = bestbid
>          where
>          bestbid = fst (ol_last b)

>lob_getbestoffer (LOB b o bw sw tr ti or sen tinfo stats) 
>        = bestoffer
>          where
>          bestoffer = fst (ol_first o)

>lob_getsentiment (LOB b o bw sw tr ti or sen tinfo stats) = sen
>lob_setsentiment sent (LOB b o bw sw tr ti or sen tinfo stats) = LOB b o bw sw tr ti or sent tinfo stats

>lob_getbuysideliquidity (LOB b o bw sw tr ti or sen tinfo stats)
>    = (ol_getliquidity b) 

>lob_getsellsideliquidity (LOB b o bw sw tr ti or sen tinfo stats)
>    = (ol_getliquidity o)

>lob_getbuysidelevels (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getlevels b

>lob_getbuysidedepth (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getdepth Bid b

>lob_getbuysidedepthneartop (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getdepthneartop Bid b

>lob_getsellsidelevels (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getlevels o

>lob_getsellsidedepth (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getdepth Offer o

>lob_getsellsidedepthneartop (LOB b o bw sw tr ti or sent tinfo stats)
>    = ol_getdepthneartop Offer o

>lob_getmidprice (LOB b o bw sw tr ti or sen tinfo stats)
>    = (x + y)/2
>      where
>      x = 0, if (isemptyorderlist b)                        || A stub bid
>        = (ol_gethighestprice b), otherwise
>      y = 0, if (isemptyorderlist o)                        || A strange stub offer 
>        = (ol_getlowestprice o), otherwise

>lob_getlasttradedprice (LOB b o bw sw tr ti or sen tinfo stats)
>    = 0, if (tr=[])
>    = order_getprice (fst (hd tr)), otherwise

>lob_gettraderinfo tid (LOB b o bw sw tr ti or sen tinfo stats)
>    = f tinfo
>      where
>      f [] = emptytraderinfo_item
>      f ((t,a,b,c):xs) = (t,a,b,c), if (t=tid)
>                       = f xs, otherwise

>lob_settraderinfo (t1,a1,b1,c1) (LOB b o bw sw tr ti or sen tinfo stats)
>    = LOB b o bw sw tr ti or sen newtinfo stats
>      where
>      newtinfo = f (t1,a1,b1,c1) tinfo
>      f (t1,a1,b1,c1) []                 = [(t1,a1,b1,c1)]
>      f (t1,a1,b1,c1) ((t1,a2,b2,c2):xs) = ((t1,a1,b1,c1):xs)
>      f (t1,a1,b1,c1) ((t2,a2,b2,c2):xs) = (t2,a2,b2,c2):(f (t1,a1,b1,c1) xs)


>lob_getordernum (LOB b o bw sw tr ti or sen tinfo stats)
>    = # or

>newbid p x (LOB b o bw sw tr ti or sen tinfo stats) 
>        = execute_trades newlob
>          where
>          newlob = LOB (ol_insert p b x) o bw sw tr ti or sen tinfo stats

>newoffer p x (LOB b o bw sw tr ti or sen tinfo stats) 
>        = execute_trades newlob
>          where
>          newlob = LOB b (ol_insert p o x) bw sw tr ti or sen tinfo stats

>neworder x (LOB b o bw sw tr ti or sen tinfo stats)
>        = (LOB b o bw sw tr ti (x:or) sen tinfo stats)
>||        = lob_increment_time (LOB b o bw sw tr ti (x:or) sen tinfo stats)

>newtrade p x (LOB b o bw sw tr ti or sen tinfo stats) 
>        = execute_trades newlob
>          where
>          newlob = LOB b o (bw++[x]) sw tr ti or sen tinfo stats, if (order_gettype x = Buy)
>                 = LOB b o bw (sw++[x]) tr ti or sen tinfo stats, otherwise

>lob_gettrades (LOB b o bw sw tr ti or sen tinfo stats) = tr

> || Matching order = 1. buys - 2. sells - 3. crossed book
>execute_trades (LOB bb oo [] [] tr ti or sen tinfo stats) 
>        = uncross_book (LOB bb oo [] [] tr ti or sen tinfo stats)
>execute_trades (LOB bb oo [] sw tr ti or sen tinfo stats) 
>        = uncross_book (LOB newbb oo [] newsw newtr ti or sen tinfo stats)
>          where
>          (newbb, newsw, newtr) = execute_sells (bb, sw, tr, []) 
>execute_trades (LOB bb oo bw [] tr ti or sen tinfo stats) 
>        = uncross_book (LOB bb newoo newbw [] newtr ti or sen tinfo stats)
>          where
>          (newoo, newbw, newtr) = execute_buys (oo, bw, tr, []) 
>execute_trades (LOB bb oo bw sw tr ti or sen tinfo stats) 
>        = uncross_book (LOB bb2 oo2 bw2 sw2 tr2 ti or sen tinfo stats)
>          where
>          (bb2, sw2, tr2) = execute_sells (bb, sw, tr1, []) 
>          (oo2, bw2, tr1) = execute_buys (oo, bw, tr, []) 
>

>execute_sells:: (bids, sells_waiting, trades_done, sells_waiting) -> (bids, sells_waiting, trades_done)
>execute_sells (bb, [], tr, notdone) 
>    = (bb, notdone, tr)
>execute_sells (bb, (s:sw), tr, notdone)                    ||take sells in time order - front of list is oldest
>    = execute_sells (newbb, sw, newtr, newnotdone)
>      where
>      (newbb, newtr, newnotdone) = (ol_clean bb1, tr1++tr, notdone), if (isemptyorder s1) || then the market order was completely filled
>                                 = (ol_clean bb1, tr1++tr, (s1:notdone)), otherwise
>      (bb1, s1, tr1) = ol_fill bb s Sell []

>execute_buys:: (offers, buys_waiting, trades_done, buys_waiting) -> (offers, buys_waiting, trades_done)
>execute_buys (oo, [], tr, notdone) = (oo, notdone, tr) 
>execute_buys (oo, (b:bw), tr, notdone)                    ||take buys in time order - front of list is oldest
>    = execute_buys (newoo, bw, newtr, newnotdone)
>      where
>      (newoo, newtr, newnotdone) = (ol_clean oo1, tr1++tr, notdone), if (isemptyorder b1) || then the market order was completely filled
>                                 = (ol_clean oo1, tr1++tr, (b1:notdone)), otherwise
>      (oo1, b1, tr1) = ol_fill oo b Buy []


>is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo stats)
>    = False, if (isemptyorderlist bb)
>    = False, if (isemptyorderlist oo)
>    = True, if (highestbidprice >= lowestsellprice)
>    = False, otherwise
>      where
>      highestbidprice = ol_gethighestprice bb
>      lowestsellprice = ol_getlowestprice oo

> || to uncross a crossed book, we pop the lowest offer and turn it into a market order for execution
> || - anything left after execution goes back as an offer, otherwise pop next offer and repeat
>uncross_book (LOB bb oo bw sw tr ti or sen tinfo stats)
>    = (LOB bb oo bw sw tr ti or sen tinfo stats), if ~(is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo stats))
>    = uncross_book (LOB newbb newoo bw sw newtr ti or sen tinfo stats), otherwise  
>      where
>      (lowest_offer, remaining_offers) = ol_poplowest oo                                              || pop the lowest offer
>      (bb1, s1, tr1) = ol_fill_cross bb lowest_offer Sell []                                          || try to execute against bids
>      newoo = ol_clean (ol_insert (order_getprice s1) remaining_offers s1), if ~(isemptyorder s1)     || anything remaining goes onto newoo
>            = ol_clean remaining_offers, otherwise
>      newbb = ol_clean bb1                                                                   || what's left after trying to match lowest offer against bids
>      newtr = tr1 ++ tr                                                                      || the old tr plus any executions


>|| The matching engine takes in an order and a start book and returns a resulting book.
>match x ob = (newbid p x (neworder x ob)), if (ty=Bid)
>           = (newoffer p x (neworder x ob)), if (ty=Offer)
>           = (newtrade p x (neworder x ob)), if or [(ty=Sell), (ty=Buy)]
>           = ob, if (ty=None)
>||           = (lob_increment_time ob), if (ty=None)
>             where
>             ty = (order_gettype x)
>             p  = (order_getprice x)


>|| The match_m1 matching engine takes in an order and a start book and returns a resulting book.
>|| Includes attenuation of takers.
>||
>match_m1 x ob = (newbid p x (neworder x ob)), if (ty=Bid)
>              = (newoffer p x (neworder x ob)), if (ty=Offer)
>              = f ty, if or [(ty=Sell), (ty=Buy)]
>              = ob, if (ty=None)
>              = ob, otherwise
>||              = (lob_increment_time ob), if (ty=None)
>||              = (lob_increment_time ob), otherwise
>                where
>                ty = order_gettype x
>                p  = order_getprice x
>                size = order_getsize x
>                bs_liquidity = lob_getbuysideliquidity ob   
>                ss_liquidity = lob_getsellsideliquidity ob
>                bs_ratio = (size / bs_liquidity), if (bs_liquidity ~=0)
>                         = 1, otherwise
>                ss_ratio = (size / ss_liquidity), if (ss_liquidity ~=0)
>                         = 1, otherwise
>                tinfo = lob_gettraderinfo (order_gettraderid x) ob
>                (tid, ltaken, penalty, timeapplied)  = tinfo
>                current_time = (lob_gettime ob) + 1
>                this_trader_being_attenuated
>                       = False, if (tinfo = emptytraderinfo_item)
>                       = True, if (current_time < (timeapplied + penalty))
>||                f Buy  = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (ss_ratio > 0.020)
>                f Buy  = (lob_settraderinfo newtinfo ob),                    if (ss_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.015)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.010)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.005)
>||                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
>                       = ob,                                                 if this_trader_being_attenuated
>                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
>                         where
>                         newtinfo = (tid, ss_ratio, penalty, current_time)
>                         penalty = 400, if (ss_ratio > 0.020)
>                         penalty = 200, if (ss_ratio > 0.015)
>                         penalty = 100, if (ss_ratio > 0.010)
>                         penalty = 50, if (ss_ratio > 0.005)
>                         penalty = 0, otherwise
>||                f Sell = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (bs_ratio > 0.020)
>                f Sell = (lob_settraderinfo newtinfo ob),                    if (bs_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.015)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.010)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.005)
>                       = ob,                                                 if this_trader_being_attenuated
>                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
>                         where
>                         newtinfo = (tid, bs_ratio, penalty, current_time)
>                         penalty = 400, if (ss_ratio > 0.020)
>                         penalty = 200, if (ss_ratio > 0.015)
>                         penalty = 100, if (ss_ratio > 0.010)
>                         penalty = 50, if (ss_ratio > 0.005)
>                         penalty = 0, otherwise

>|| The match_2 matching engine extends match_m1 by repricing all bids > 35% away from current best bid and all
>|| offers > 35% away from current best offer.
>match_m2 x ob = checkbid,   if (ty=Bid)
>              = checkoffer, if (ty=Offer)
>              = match_m1 x ob, otherwise
>                where
>                ty         = order_gettype x
>                p          = order_getprice x
>                checkbid   = match_m1 x ob, if or[(bestbid=0),(bidgap <= 0.35)]
>                           = match_m1 (order_setprice (bestbid*1.35) x) ob, if (p > bestbid)
>                           = match_m1 (order_setprice (bestbid*0.65) x) ob, otherwise
>                           ||= (lob_increment_time ob), otherwise
>                checkoffer = match_m1 x ob, if or[(bestoffer=0),(offergap <= 0.35)]
>                           = match_m1 (order_setprice (bestoffer*0.65) x) ob, if (p < bestoffer)
>                           = match_m1 (order_setprice (bestoffer*1.35) x) ob, otherwise
>                           ||= (lob_increment_time ob), otherwise
>                bestbid    = lob_getbestbid ob
>                bestoffer  = lob_getbestoffer ob
>                bidgap     = abs(bestbid - p) / bestbid, if bestbid ~= 0
>                           = 0.01, otherwise
>                offergap   = abs(bestoffer - p) / bestoffer, if bestoffer ~= 0
>                           = 0.01, otherwise

>|| The match_m3 matching engine is similar to match_m1 but stricter in that it uses the ratio against the available depth
>|| at the top of the book.
>match_m3 x ob = (newbid p x (neworder x ob)), if (ty=Bid)
>              = (newoffer p x (neworder x ob)), if (ty=Offer)
>              = f ty, if or [(ty=Sell), (ty=Buy)]
>              = ob, otherwise
>||              = (lob_increment_time ob), otherwise
>                where
>                ty = order_gettype x
>                p  = order_getprice x
>                size = order_getsize x
>                bs_liquidity = lob_getbuysidedepthneartop ob   
>                ss_liquidity = lob_getsellsidedepthneartop ob
>                ss_low = (ss_liquidity < bs_liquidity) & (((bs_liquidity - ss_liquidity)/max[bs_liquidity,1]) > 0.25)
>                bs_low = (bs_liquidity < ss_liquidity) & (((ss_liquidity - bs_liquidity)/max[ss_liquidity,1]) > 0.25)
>                bs_ratio = (size / bs_liquidity), if (bs_liquidity ~=0)
>                         = 1, otherwise
>                ss_ratio = (size / ss_liquidity), if (ss_liquidity ~=0)
>                         = 1, otherwise
>                tinfo = lob_gettraderinfo (order_gettraderid x) ob
>                (tid, ltaken, penalty, timeapplied)  = tinfo
>                current_time = (lob_gettime ob) + 1
>                this_trader_being_attenuated
>                       = False, if (tinfo = emptytraderinfo_item)
>                       = True, if (current_time < (timeapplied + penalty))
>||                f Buy  = (lob_increment_time ob),                                                 if (ss_low)
>                f Buy  = ob,                                                 if (ss_low)
>||                       = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (ss_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo ob),                    if (ss_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.005)
>||                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
>                       = ob,                                                 if this_trader_being_attenuated
>                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
>                         where
>                         newtinfo = (tid, ss_ratio, penalty, current_time)
>                         penalty = penalty_function ss_ratio
>||                f Sell = (lob_increment_time ob),                                                 if (bs_low)
>                f Sell = ob,                                                 if (bs_low)
>||                       = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (bs_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo ob),                    if (bs_ratio > 0.020)
>                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.005)
>||                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
>                       = ob,                                                 if this_trader_being_attenuated
>                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
>                         where
>                         newtinfo = (tid, bs_ratio, penalty, current_time)
>                         penalty = penalty_function bs_ratio

>|| The penalty function determines the time attenuation to be applied to a trader according to the ratio of the size of the order to the available liquidity
>penalty_function::num ->num
>penalty_function ratio = 0, if (ratio <= 0.005)
>                       = entier(20*exp(ratio*150)), otherwise

>f = penalty_function



>showlob (LOB b o bw sw tr ti or sen tinfo stats)
>    = "Limit Order Book at time t="++(show ti)++" with current value="++(show cv)++"\n-----------------------------\n"++
>      "Offers:\n" ++ (show o) ++ "\n" ++
>      "Bids:\n" ++ (show b) ++ "\n" ++
>      "Buys Waiting: " ++ (show bw) ++ "\n" ++
>      "Sells Waiting: " ++ (show sw) ++ "\n" ++
>      "Trades Done: " ++ (show tr) ++ "\n" ++
>      "Orders Received: " ++ (show or) ++ "\n" ++
>      "Sentiment: " ++ (show sen) ++ "\n"
>      where
>      (cv,a1,a2,a3,a4,a5,a6,a7,a8,a9) = stats

============

