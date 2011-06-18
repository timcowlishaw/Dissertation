|| This is a literate script - LIBRARY ROUTINES
|| Limit Order Book
|| Copyright Christopher D. Clack 2011
%include "../lib/sim-orderlist.m" 

||====================
||LIMIT ORDER BOOK

abstype lob
with
    emptylob :: lob
    lob_increment_time :: lob -> lob 
    lob_gettime :: lob -> time
    lob_getbestbid :: lob -> price
    lob_getbestoffer :: lob -> price
    lob_getsentiment :: lob -> sentiment
    lob_setsentiment :: sentiment -> lob -> lob
    lob_getbuysideliquidity :: lob -> size
    lob_getsellsideliquidity :: lob -> size
    lob_getmidprice :: lob -> price
    lob_getlasttradedprice :: lob -> price
    lob_getordernum :: lob -> num
    lob_getbuysidelevels :: lob -> num
    lob_getbuysidedepth :: lob -> num
    lob_getbuysidedepthneartop :: lob -> num
    lob_getsellsidelevels :: lob -> num
    lob_getsellsidedepth :: lob -> num
    lob_getsellsidedepthneartop :: lob -> num
    lob_gettraderinfo :: traderid -> lob -> traderinfo_item
    lob_settraderinfo :: traderinfo_item -> lob -> lob
    lob_gettrace :: lob -> [char]
    newbid :: price -> order -> lob -> lob
    newoffer :: price -> order -> lob -> lob
    newtrade :: price -> order -> lob -> lob
    neworder :: order -> lob -> lob
    lob_gettrades :: lob -> trades_done
    execute_trades :: lob -> lob
    is_crossed_book :: lob -> bool
    uncross_book :: lob -> lob
    match :: order -> lob -> lob
    match_m1 :: order -> lob -> lob
    showlob :: lob -> [char]


|| Implementation of lob
lob == underlyinglob
underlyinglob ::= LOB bids offers buys_waiting sells_waiting trades_done time orders_received sentiment traderinfo
lobtrace == [[char]]
bids == orderlist
offers == orderlist
buys_waiting == [order]
sells_waiting == [order]
trades_done == [(order, order)]
orders_received == [order]
traderinfo == [traderinfo_item]
traderinfo_item == (traderid, percent_liquidity_taken, penalty_applied, time)   || time is time penalty was applied
percent_liquidity_taken == num
penalty_applied == num

|| We add two orders into the empty order book to "seed" it for stability (otherwise we have to wait several ticks for the book to stabililze)
emptylob = LOB emptyorderlist emptyorderlist 
||               (ol_insert ((value Calm 0)*0.9) emptyorderlist (order_create ((value Calm 0)*0.9) 500 0 Phantom Bid))
||               (ol_insert ((value Calm 0)*1.1) emptyorderlist (order_create ((value Calm 0)*1.1) 500 0 Phantom Offer))
               [] [] [] 0 [] Calm []

emptytraderinfo_item = (Phantom, 0, 0, 0)

lob_increment_time (LOB b o bw sw tr ti or sen tinfo) 
    = LOB b o bw sw tr (ti+1) or sen tinfo 

lob_gettrace ob
    = (show (lob_gettime ob)) ++ "," ++			         || Time
      (show (lob_getbuysideliquidity ob)) ++ "," ++                     || Buy Side Liquidity
      (show (lob_getsellsideliquidity ob)) ++ "," ++                    || Sell Side Liquidity
      ||   (show (lob_getmidprice ob)) ++ "," ++                        || Mid Price
      (show (lob_getbestbid ob)) ++ "," ++                              || Best Bid
      (show (lob_getbestoffer ob)) ++ "," ++                            || Best Offer 
      (show (ltp)) ++ "," ++                                            || Last Traded Price
      (show (# (lob_gettrades ob)))  ++ "," ++                          || Cumulative Trades
      (show (lob_getbuysidelevels ob))  ++ "," ++                       || Buy Side Levels 
      (show (lob_getsellsidelevels ob))  ++","++                        || Sell Side Levels
      (show (lob_getbuysidedepth ob))  ++","++                          || Buy Side Depth
      (show (lob_getsellsidedepth ob))  ++","++                         || Sell Side Depth
      (show current_value) ++ "," ++                                    || Current value
      (show (lob_getbuysidedepthneartop ob))  ++","++                   || Buy Side Depth Near Top
      (show (lob_getsellsidedepthneartop ob))  ++","++                  || Sell Side Depth Near Top
      (show ((ltp - current_value)^2))  ++                              || Squared error LTP-UV
       "\n"
      where
      current_value = value (lob_getsentiment ob) (lob_gettime ob)
      ltp = lob_getlasttradedprice ob

lob_gettime (LOB b o bw sw tr ti or sen tinfo) = ti
lob_getbestbid (LOB b o bw sw tr ti or sen tinfo) 
        = bestbid
          where
          bestbid = fst (ol_last b)

lob_getbestoffer (LOB b o bw sw tr ti or sen tinfo) 
        = bestoffer
          where
          bestoffer = fst (ol_first o)

lob_getsentiment (LOB b o bw sw tr ti or sen tinfo) = sen
lob_setsentiment sent (LOB b o bw sw tr ti or sen tinfo) = LOB b o bw sw tr ti or sent tinfo

lob_getbuysideliquidity (LOB b o bw sw tr ti or sen tinfo)
    = (ol_getliquidity b) 

lob_getsellsideliquidity (LOB b o bw sw tr ti or sen tinfo)
    = (ol_getliquidity o)

lob_getbuysidelevels (LOB b o bw sw tr ti or sent tinfo)
    = ol_getlevels b

lob_getbuysidedepth (LOB b o bw sw tr ti or sent tinfo)
    = ol_getdepth Bid b

lob_getbuysidedepthneartop (LOB b o bw sw tr ti or sent tinfo)
    = ol_getdepthneartop Bid b

lob_getsellsidelevels (LOB b o bw sw tr ti or sent tinfo)
    = ol_getlevels o

lob_getsellsidedepth (LOB b o bw sw tr ti or sent tinfo)
    = ol_getdepth Offer o

lob_getsellsidedepthneartop (LOB b o bw sw tr ti or sent tinfo)
    = ol_getdepthneartop Offer o

lob_getmidprice (LOB b o bw sw tr ti or sen tinfo)
    = (x + y)/2
      where
      x = 0, if (isemptyorderlist b)                        || A stub bid
        = (ol_gethighestprice b), otherwise
      y = 0, if (isemptyorderlist o)                        || A strange stub offer 
        = (ol_getlowestprice o), otherwise

lob_getlasttradedprice (LOB b o bw sw tr ti or sen tinfo)
    = 0, if (tr=[])
    = order_getprice (fst (hd tr)), otherwise

lob_gettraderinfo tid (LOB b o bw sw tr ti or sen tinfo)
    = f tinfo
      where
      f [] = emptytraderinfo_item
      f ((t,a,b,c):xs) = (t,a,b,c), if (t=tid)
                       = f xs, otherwise

lob_settraderinfo (t1,a1,b1,c1) (LOB b o bw sw tr ti or sen tinfo)
    = LOB b o bw sw tr ti or sen newtinfo
      where
      newtinfo = f (t1,a1,b1,c1) tinfo
      f (t1,a1,b1,c1) []                 = [(t1,a1,b1,c1)]
      f (t1,a1,b1,c1) ((t1,a2,b2,c2):xs) = ((t1,a1,b1,c1):xs)
      f (t1,a1,b1,c1) ((t2,a2,b2,c2):xs) = (t2,a2,b2,c2):(f (t1,a1,b1,c1) xs)


lob_getordernum (LOB b o bw sw tr ti or sen tinfo)
    = # or

newbid p x (LOB b o bw sw tr ti or sen tinfo) 
        = execute_trades newlob
          where
          newlob = LOB (ol_insert p b x) o bw sw tr ti or sen tinfo

newoffer p x (LOB b o bw sw tr ti or sen tinfo) 
        = execute_trades newlob
          where
          newlob = LOB b (ol_insert p o x) bw sw tr ti or sen tinfo

neworder x (LOB b o bw sw tr ti or sen tinfo)
        = lob_increment_time (LOB b o bw sw tr ti (x:or) sen tinfo)

newtrade p x (LOB b o bw sw tr ti or sen tinfo) 
        = execute_trades newlob
          where
          newlob = LOB b o (bw++[x]) sw tr ti or sen tinfo, if (order_gettype x = Buy)
                 = LOB b o bw (sw++[x]) tr ti or sen tinfo, otherwise

lob_gettrades (LOB b o bw sw tr ti or sen tinfo) = tr

 || Matching order = 1. buys - 2. sells - 3. crossed book
execute_trades (LOB bb oo [] [] tr ti or sen tinfo) = uncross_book (LOB bb oo [] [] tr ti or sen tinfo)
execute_trades (LOB bb oo [] sw tr ti or sen tinfo) = uncross_book (LOB newbb oo [] newsw newtr ti or sen tinfo)
                                                      where
                                                      (newbb, newsw, newtr) = execute_sells (bb, sw, tr, []) 
execute_trades (LOB bb oo bw [] tr ti or sen tinfo) = uncross_book (LOB bb newoo newbw [] newtr ti or sen tinfo)
                                                      where
                                                      (newoo, newbw, newtr) = execute_buys (oo, bw, tr, []) 
execute_trades (LOB bb oo bw sw tr ti or sen tinfo) = uncross_book (LOB bb2 oo2 bw2 sw2 tr2 ti or sen tinfo)
                                                      where
                                                      (bb2, sw2, tr2) = execute_sells (bb, sw, tr1, []) 
                                                      (oo2, bw2, tr1) = execute_buys (oo, bw, tr, []) 


execute_sells:: (bids, sells_waiting, trades_done, sells_waiting) -> (bids, sells_waiting, trades_done)
execute_sells (bb, [], tr, notdone) = (bb, notdone, tr)
execute_sells (bb, (s:sw), tr, notdone)                    ||take sells in time order - front of list is oldest
    = execute_sells (newbb, sw, newtr, newnotdone)
      where
      (newbb, newtr, newnotdone) = (ol_clean bb1, tr1++tr, notdone), if (isemptyorder s1) || then the market order was completely filled
                                 = (ol_clean bb1, tr1++tr, (s1:notdone)), otherwise
      (bb1, s1, tr1) = ol_fill bb s Sell []

execute_buys:: (offers, buys_waiting, trades_done, buys_waiting) -> (offers, buys_waiting, trades_done)
execute_buys (oo, [], tr, notdone) = (oo, notdone, tr) 
execute_buys (oo, (b:bw), tr, notdone)                    ||take buys in time order - front of list is oldest
    = execute_buys (newoo, bw, newtr, newnotdone)
      where
      (newoo, newtr, newnotdone) = (ol_clean oo1, tr1++tr, notdone), if (isemptyorder b1) || then the market order was completely filled
                                 = (ol_clean oo1, tr1++tr, (b1:notdone)), otherwise
      (oo1, b1, tr1) = ol_fill oo b Buy []


is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo)
    = False, if (isemptyorderlist bb)
    = False, if (isemptyorderlist oo)
    = True, if (highestbidprice >= lowestsellprice)
    = False, otherwise
      where
      highestbidprice = ol_gethighestprice bb
      lowestsellprice = ol_getlowestprice oo

 || to uncross a crossed book, we pop the lowest offer and turn it into a market order for execution
 || - anything left after execution goes back as an offer, otherwise pop next offer and repeat
uncross_book (LOB bb oo bw sw tr ti or sen tinfo)
    = (LOB bb oo bw sw tr ti or sen tinfo), if ~(is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo))
    = uncross_book (LOB newbb newoo bw sw newtr ti or sen tinfo), otherwise  
      where
      (lowest_offer, remaining_offers) = ol_poplowest oo                                              || pop the lowest offer
      (bb1, s1, tr1) = ol_fill_cross bb lowest_offer Sell []                                          || try to execute against bids
      newoo = ol_clean (ol_insert (order_getprice s1) remaining_offers s1), if ~(isemptyorder s1)     || anything remaining goes onto newoo
            = ol_clean remaining_offers, otherwise
      newbb = ol_clean bb1                                                                   || what's left after trying to match lowest offer against bids
      newtr = tr1 ++ tr                                                                      || the old tr plus any executions


|| The matching engine takes in an order and a start book and returns a resulting book.
match x ob = (newbid p x (neworder x ob)), if (ty=Bid)
           = (newoffer p x (neworder x ob)), if (ty=Offer)
           = (newtrade p x (neworder x ob)), if or [(ty=Sell), (ty=Buy)]
           = (lob_increment_time ob), if (ty=None)
             where
             ty = (order_gettype x)
             p  = (order_getprice x)


|| The match_m1 matching engine takes in an order and a start book and returns a resulting book.
|| Includes attenuation of takers.
||
match_m1 x ob = (newbid p x (neworder x ob)), if (ty=Bid)
              = (newoffer p x (neworder x ob)), if (ty=Offer)
              = f ty, if or [(ty=Sell), (ty=Buy)]
              = (lob_increment_time ob), if (ty=None)
              = (lob_increment_time ob), otherwise
                where
                ty = order_gettype x
                p  = order_getprice x
                size = order_getsize x
                bs_liquidity = lob_getbuysideliquidity ob   
                ss_liquidity = lob_getsellsideliquidity ob
                bs_ratio = (size / bs_liquidity), if (bs_liquidity ~=0)
                         = 1, otherwise
                ss_ratio = (size / ss_liquidity), if (ss_liquidity ~=0)
                         = 1, otherwise
                tinfo = lob_gettraderinfo (order_gettraderid x) ob
                (tid, ltaken, penalty, timeapplied)  = tinfo
                current_time = (lob_gettime ob) + 1
                this_trader_being_attenuated
                       = False, if (tinfo = emptytraderinfo_item)
                       = True, if (current_time < (timeapplied + penalty))
                f Buy  = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (ss_ratio > 0.020)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.015)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.010)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.005)
                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
                         where
                         newtinfo = (tid, ss_ratio, penalty, current_time)
                         penalty = 400, if (ss_ratio > 0.020)
                         penalty = 200, if (ss_ratio > 0.015)
                         penalty = 100, if (ss_ratio > 0.010)
                         penalty = 50, if (ss_ratio > 0.005)
                         penalty = 0, otherwise
                f Sell = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (bs_ratio > 0.020)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.015)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.010)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.005)
                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
                         where
                         newtinfo = (tid, bs_ratio, penalty, current_time)
                         penalty = 400, if (ss_ratio > 0.020)
                         penalty = 200, if (ss_ratio > 0.015)
                         penalty = 100, if (ss_ratio > 0.010)
                         penalty = 50, if (ss_ratio > 0.005)
                         penalty = 0, otherwise

|| The match_2 matching engine extends match_m1 by repricing all bids > 35% away from current best bid and all
|| offers > 35% away from current best offer.
match_m2 x ob = checkbid,   if (ty=Bid)
              = checkoffer, if (ty=Offer)
              = match_m1 x ob, otherwise
                where
                ty         = order_gettype x
                p          = order_getprice x
                checkbid   = match_m1 x ob, if or[(bestbid=0),(bidgap <= 0.35)]
                           = match_m1 (order_setprice (bestbid*1.35) x) ob, if (p > bestbid)
                           = match_m1 (order_setprice (bestbid*0.65) x) ob, otherwise
                           ||= (lob_increment_time ob), otherwise
                checkoffer = match_m1 x ob, if or[(bestoffer=0),(offergap <= 0.35)]
                           = match_m1 (order_setprice (bestoffer*0.65) x) ob, if (p < bestoffer)
                           = match_m1 (order_setprice (bestoffer*1.35) x) ob, otherwise
                           ||= (lob_increment_time ob), otherwise
                bestbid    = lob_getbestbid ob
                bestoffer  = lob_getbestoffer ob
                bidgap     = abs(bestbid - p) / bestbid, if bestbid ~= 0
                           = 0.01, otherwise
                offergap   = abs(bestoffer - p) / bestoffer, if bestoffer ~= 0
                           = 0.01, otherwise

|| The match_m3 matching engine is similar to match_m1 but stricter in that it uses the ratio against the available depth
|| at the top of the book.
match_m3 x ob = (newbid p x (neworder x ob)), if (ty=Bid)
              = (newoffer p x (neworder x ob)), if (ty=Offer)
              = f ty, if or [(ty=Sell), (ty=Buy)]
              = (lob_increment_time ob), otherwise
                where
                ty = order_gettype x
                p  = order_getprice x
                size = order_getsize x
                bs_liquidity = lob_getbuysidedepthneartop ob   
                ss_liquidity = lob_getsellsidedepthneartop ob
                ss_low = (ss_liquidity < bs_liquidity) & (((bs_liquidity - ss_liquidity)/max[bs_liquidity,1]) > 0.25)
                bs_low = (bs_liquidity < ss_liquidity) & (((ss_liquidity - bs_liquidity)/max[ss_liquidity,1]) > 0.25)
                bs_ratio = (size / bs_liquidity), if (bs_liquidity ~=0)
                         = 1, otherwise
                ss_ratio = (size / ss_liquidity), if (ss_liquidity ~=0)
                         = 1, otherwise
                tinfo = lob_gettraderinfo (order_gettraderid x) ob
                (tid, ltaken, penalty, timeapplied)  = tinfo
                current_time = (lob_gettime ob) + 1
                this_trader_being_attenuated
                       = False, if (tinfo = emptytraderinfo_item)
                       = True, if (current_time < (timeapplied + penalty))
                f Buy  = (lob_increment_time ob),                                                 if (ss_low)
                       = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (ss_ratio > 0.020)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (ss_ratio > 0.005)
                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
                         where
                         newtinfo = (tid, ss_ratio, penalty, current_time)
                         penalty = 400, if (ss_ratio > 0.020)
                         penalty = 200, if (ss_ratio > 0.015)
                         penalty = 200, if (ss_ratio > 0.010)
                         penalty = 50, if (ss_ratio > 0.005)
                         penalty = 0, otherwise
                f Sell = (lob_increment_time ob),                                                 if (bs_low)
                       = (lob_settraderinfo newtinfo (lob_increment_time ob)),                    if (bs_ratio > 0.020)
                       = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob))),             if (bs_ratio > 0.005)
                       = (lob_increment_time ob),                                                 if this_trader_being_attenuated
                       = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))), otherwise
                         where
                         newtinfo = (tid, bs_ratio, penalty, current_time)
                         penalty = 400, if (ss_ratio > 0.020)
                         penalty = 200, if (ss_ratio > 0.015)
                         penalty = 200, if (ss_ratio > 0.010)
                         penalty = 50, if (ss_ratio > 0.005)
                         penalty = 0, otherwise




showlob (LOB b o bw sw tr ti or sen tinfo)
    = "Limit Order Book at time t="++(show ti)++"\n-----------------------------\n"++
      "Offers:\n" ++ (show o) ++ "\n" ++
      "Bids:\n" ++ (show b) ++ "\n" ++
      "Buys Waiting: " ++ (show bw) ++ "\n" ++
      "Sells Waiting: " ++ (show sw) ++ "\n" ++
      "Trades Done: " ++ (show tr) ++ "\n" ++
      "Orders Received: " ++ (show or) ++ "\n" ++
      "Sentiment: " ++ (show sen) ++ "\n"

||============

