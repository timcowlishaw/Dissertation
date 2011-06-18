>|| This is a literate script - VERSION 4 - MODIFICATION 1
>|| Copyright Christopher D. Clack 2011
>|| Attenuation of takers
>%include "../lib/sim-orderlist.m"
>%include "../lib/sim-lob.m"


This is a simulator for the self-stabilizing order book.

The experiments we wish to run are:

1) Given pre-prepared inputs that are designed to cause a run on the book, how stable are prices on the book? (fairly easy)

2) Given pre-prepared inputs that are designed to flood the book with high volumes, how stable are volumes on the book?  (fairly easy)

3) Given pre-prepared inputs that are designed to scare away fundamental liquidity, how much fundamental liquidity remains on the book (more dificult)




=====================================
TRADERS

>
>traderstate == (num, num, num)
>emptytraderstate = (0, 0, 0)

>|| A generic trader takes as arguments:
>||    - a catid comprising a constructor giving the category of the trader and an ID number
>||    - a list of random numbers
>||    - a list of numbers with gaussian distribution 
>||    - a list of time-ordered snapshots of the limit order book - only look at the head of this list!
>||    - a traderstate holding state information only of interest to this instance of the trader
>|| And the retruned result is a list of orders to be sent to the exchange
>||
>|| NOT DONE YET !!!
>|| - use the gaussians as offset for orders
>|| - use the sentiment held in the LOB to determine how aggressively to trade
>|| - store (cat id) in orders, not just id

>generic_trader :: traderid -> [num] -> [num] -> [lob] -> traderstate -> [order]
>generic_trader catid (r:rs) (g:gs) [] state = []
>generic_trader catid [] (g:gs) (x:xs) state = error "generic trader: run out of random numbers"
>generic_trader catid (r:rs) [] (x:xs) state = error "generic trader: run out of gaussian numbers"
>generic_trader catid (r:rs) (g:gs) (x:xs) (old_bestbid, old_bestoffer, old_ordernum)
>    = [], if time > 980
>    = (d1 catid sent) : (generic_trader catid rs gs xs newstate), otherwise
>      where
>      newstate = (bestbid, bestoffer, ordernum)
>      ordernum = lob_getordernum x
>      ordernum_move = (ordernum - old_ordernum) / old_ordernum, if (old_ordernum ~= 0)
>                    = 0.05, otherwise
>      time = lob_gettime x
>      bestbid = lob_getbestbid x
>      bestoffer = lob_getbestoffer x
>      ||midprice = (bestbid + bestoffer)/2
>      bidpricemove = (bestbid - old_bestbid) / old_bestbid, if (old_bestbid ~= 0)
>                   = 0.05, otherwise
>      offerpricemove = (bestoffer - old_bestoffer) / old_bestoffer, if (old_bestoffer ~= 0)
>                   = 0.05, otherwise
>      my_inventory = foldr (+) 0 (map getmovement mytrades)
>                     where
>                     mytrades = filter is_mine flattenedtrades
>                     flattenedtrades = expand (lob_gettrades x)
>                     expand [] = []
>                     expand ((a,b):xs) = a:b:(expand xs)
>                     is_mine o = ((order_gettraderid o) = catid)
>                     getmovement o = osize, if (or [ otype = Bid, otype = Buy])
>                                   = (- osize), otherwise
>                                     where
>                                     osize = order_getsize o
>                                     otype = order_gettype o
>      sent = lob_getsentiment x
>      bs_liquidity_alert = (lob_getbuysideliquidity x) < 10
>      ss_liquidity_alert = (lob_getsellsideliquidity x) < 10
>      crossed_book_alert = (lob_getbestbid x) > (lob_getbestoffer x)
>      bs_levels_alert = (lob_getbuysidelevels x) < 3
>      ss_levels_alert = (lob_getsellsidelevels x) < 3
>      current_value = value sent time
>      offerpremium = current_value*0.05   || if premium set as percentage of value, ramp is stable
>      bidpremium = current_value*0.05
>      newbidprice = max[(current_value - bidpremium),0]  || entier (bestbid + (g*20))
>      newofferprice = max [(current_value + offerpremium),0] || entier (bestoffer - (g*20))
>      newbuysize  = 100 || entier (abs ((r mod 500) + (g*10)))
>      newsellsize = 100 || entier (abs ((r mod 500) + (g*1000)))
>      fillsperorder = 1.5
>||
>||************************************************************************************************
>|| VERSION 3 has feedback loops
>||************************************************************************************************
>||
>|| Fundamental buyers are informed by the underlying value of the security. They monitor quote volumes (ordernum_move)
>|| and withdraw liquidity when they are high
>      d1 (FundBuyer id) sent       = order_create newbidprice z time catid Bid, if (bestoffer = 0)
>                                   = order_create (min[bestoffer,bestbid,newbidprice]) z time catid Bid, if and[(r < 500), (ordernum_move < 0.1)]
>                                   = order_create (min[bestoffer,bestbid,newbidprice]) (z*(1-ordernum_move)) time catid Bid, if and[(r < 500), (ordernum_move < 0.3)]
>                                   = emptyorder, otherwise
>                                     where
>                                     ||target = 5000
>                                     z = (fillsperorder * newsellsize) || max [target-my_inventory,50]
>|| Fundamental sellers are informed by the underlying value of the security. They monitor quote volumes (ordernum_move)
>|| and withdraw liquidity when they are high
>      d1 (FundSeller id) sent      = order_create newofferprice z time catid Offer, if (bestbid = 0)
>                                   = order_create (max[bestbid, bestoffer,newofferprice]) z time catid Offer, if and[(or[(r < 700),(time < 100)]),(ordernum_move < 0.1)]
>                                   = order_create (max[bestbid, bestoffer,newofferprice]) (z*(1-ordernum_move)) time catid Offer, if and[(or[(r < 700),(time < 100)]),(ordernum_move < 0.3)]
>                                   = emptyorder, otherwise
>                                     where
>                                     ||target = 5000
>                                     z = fillsperorder * newbuysize || min [abs (my_inventory - target),50]
>|| Intermediaries issue both Bids and Offers at the current BBO or just behind (since they can place bigger orders)
>      d1 (Intermediary id) sent    = order_create newbidprice (fillsperorder*newsellsize) time catid Bid, if (bestbid = 0)
>                                   = order_create newofferprice (fillsperorder*newbuysize) time catid Offer, if (bestoffer = 0)
>                                   = order_create newofferprice newbuysize time catid Offer, if or[ss_liquidity_alert,ss_levels_alert, crossed_book_alert, r<500]
>                                   = order_create newbidprice   newsellsize  time catid Bid,   if or[(my_inventory < 50),bs_liquidity_alert,bs_levels_alert, r>=500]
>                                   = emptyorder, otherwise
>                                     ||where
>                                     ||target = 50000
>                                     ||temp = target - my_inventory
>                                     ||z = newsellsize || min [(abs temp),500]
>|| HFTs place bids and offers at the current BBO or Sell aggressively if they exceed their target inventory
>      d1 (HFT id) sent             = order_create newbidprice (fillsperorder*newsellsize) time catid Bid, if (bestbid = 0)
>                                   = order_create newofferprice (fillsperorder*newbuysize) time catid Offer, if (bestoffer = 0)
>                                   = order_create newbidprice   bsize time catid Bid,   if (my_inventory < target)
>                                   = order_create newofferprice osize time catid Offer, if (my_inventory > osize)
>                                   = order_create 0         my_inventory time catid Sell,  if (my_inventory > target)
>                                     where
>                                     osize = fillsperorder * newbuysize
>                                     bsize = fillsperorder * newsellsize
>                                     target = 1000
>                                     ||z = newsellsize || min [abs (target-my_inventory),50]
>||
>|| Small traders mostly do not trade at all, and when they do trade it is essentially random
>|| BSTakers/SSTaker traders (buyers/sellers) change order size if price movements exceed certain limits.  
>|| Tiny movements are ignored, small movements increase size, bigger movements decrease size down to 0 for fear of buying too expensively
>|| or selling too cheaply
>||
>      d1 (Small id) sent           = emptyorder, if (r < 900)
>                                   = order_create 0 mysellsize time catid Sell, if (r<800)    || same as SSTaker
>                                   = order_create 0 mybuysize time catid Buy, otherwise       || same as BSTaker
>                                     where
>                                     mysellsize = newbuysize, if ((abs bidpricemove) < 0.05)
>                                                = newbuysize * (1+(abs bidpricemove)), if ((bidpricemove > 0) & (bidpricemove < 0.1))
>                                                = newbuysize * (1-(abs bidpricemove)), if ((bidpricemove > 0) & (bidpricemove < 0.4))
>                                                = 0, if (bidpricemove > 0)
>                                                = 0, otherwise
>                                     mybuysize  = newbuysize, if ((abs offerpricemove) < 0.05)
>                                                = newbuysize * (1+(abs offerpricemove)), if ((offerpricemove < 0) & (offerpricemove > (-0.1)))
>                                                = newbuysize * (1-(abs offerpricemove)), if ((offerpricemove < 0) & (offerpricemove > (-0.4)))
>                                                = 0, if (offerpricemove < 0)
>                                                = 0, otherwise
>|| Opportunistic traders make small orders where they can see value.  they all are subject to the same feedback loops as Small traders
>      d1 (Opportunistic id) sent   = order_create 0 mysellsize time catid Sell, if (bestbid > current_value)
>                                   = order_create 0 mybuysize time catid Buy, if (bestoffer < current_value)
>                                   = order_create 0 mysellsize  time catid Sell,  if (r<320)
>                                   = emptyorder, otherwise
>                                     where
>                                     mysellsize = newbuysize, if ((abs bidpricemove) < 0.05)
>                                                = newbuysize * (1+(abs bidpricemove)), if ((bidpricemove > 0) & (bidpricemove < 0.1))
>                                                = newbuysize * (1-(abs bidpricemove)), if ((bidpricemove > 0) & (bidpricemove < 0.4))
>                                                = 0, if (bidpricemove > 0)
>                                                = 0, otherwise
>                                     mybuysize  = newbuysize, if ((abs offerpricemove) < 0.05) 
>                                                = newbuysize * (1+(abs offerpricemove)), if ((offerpricemove < 0) & (offerpricemove > (-0.1)))
>                                                = newbuysize * (1-(abs offerpricemove)), if ((offerpricemove < 0) & (offerpricemove > (-0.4)))
>                                                = 0, if (offerpricemove < 0)
>                                                = 0, otherwise
>||
>||************************************************************************************************
>||VERSIONS 1 and 2 only use makers and takers
>||************************************************************************************************
>||
>|| MAKERS
>||
>|| In a Calm market BSMaker traders make bids 
>      d1 (BSMaker id) sent         = order_create newbidprice (fillsperorder*newsellsize) time catid Bid
>|| In a Calm market SSMaker traders make offers
>      d1 (SSMaker id) sent         = order_create newofferprice (fillsperorder*newsellsize) time catid Offer
>|| In a Calm market Maker traders make bids and offers
>      d1 (Maker id) sent           = order_create newbidprice (fillsperorder*newsellsize) time catid Bid, if (r<500)
>                                   = order_create newofferprice (fillsperorder*newbuysize) time catid Offer,  otherwise
>||
>|| TAKERS
>||
>|| In a Calm market BSTaker traders make buys 
>      d1 (BSTaker id) sent         = order_create 0 newbuysize  time catid Buy
>|| In a Calm market SSTaker traders make sells
>      d1 (SSTaker id) sent         = order_create 0 newsellsize time catid Sell
>|| In a Calm market Taker traders make buys and sells
>      d1 (Taker id) sent           = order_create 0 newsellsize time catid Sell, if (r < 500)
>                                   = order_create 0 newbuysize  time catid Buy,  otherwise
>generic_trader catid (r:rs) (g:gs) (x:xs) state = error "Unrecognised trader category"

=====================================




============
TESTS

>d1 = order_create 10 200 1 (FundBuyer 13) Bid
>d2 = order_create 10 200 2 (FundBuyer 24) Offer
>d3 = order_create 0 200 3 (FundSeller 25) Sell
>d4 = order_create 0 200 4 (FundBuyer 24) Buy
>d5 = order_create 5 200 5 (HFT 200) Bid
>d6 = order_create 0 100 6 (HFT 200) Buy
>d7 = order_create 0 100 6 (HFT 200) Buy
>
>tester :: [sys_message]
>tester = dialogue $- 0 emptylob
>
>dialogue :: [char] -> time -> lob -> [sys_message]
>dialogue ip t ob
>    = [Tofile "trace" ((show ob) ++ "\n==========================================\n")] ++ [System "clear"] ++ [Stdout (show ob)] ++ (next sel)
>      where
>      (sel, rest) = split ip
>      split [] = ([], [])
>      split ('\n':rest) = ([], rest)
>      split (x:rest) = (x:a, b)
>                        where
>                        (a,b) = split rest
>      next ('q':[]) = (Stdout "END OF TEST\n"):[Exit 0]
>      next ('B':'u':'y':' ':x:[]) = dialogue rest (t+1) (match (order_create 0 (numval [x]) t (FundBuyer 10) Buy) ob)
>      next ('S':'e':'l':'l':' ':x:[]) = dialogue rest (t+1) (match (order_create 0 (numval [x]) t (FundSeller 2) Sell) ob)
>      next ('B':'i':'d':' ':x:' ':y:[]) = dialogue rest (t+1) (match (order_create (numval [y]) (numval [x]) t (FundSeller 2) Bid) ob)
>      next ('O':'f':'f':'e':'r':' ':x:' ':y:[]) = dialogue rest (t+1) (match (order_create (numval [y]) (numval [x]) t (FundSeller 2) Offer) ob)
>      next any   = (Stdout "END OF TEST\n"):[Exit 0]

>test_data = map f [1,2,3,4,5,6,7,8,9,10]
>            where
>            f n = order_create n n n (FundSeller n) Bid



>main :: [sys_message]
>main = mainloop 0 emptylob 

>mainloop :: time -> lob -> [sys_message]
>mainloop t ob 
>    = [Tofile "trace" ((show ob) ++ "\n=======================================\n")] ++ (next)
>      where
>      next = (Tofile "trace" "END OF TEST\n"):[Exit 0], if or [(order_gettype x) = Abort, (order_gettype y) = Abort]
>           = (Tofile "trace" "END OF TEST\n"):[Exit 0], if (t > 10)
>           = mainloop (t+2) ob2, otherwise
>             where
>             x = trader1 t ob
>             y = trader2 t ob
>             ob2 = match y ob1 
>             ob1 = match x ob 
>      trader1 t ob = d1
>      trader2 t ob = d2





>|| sim is the simulator we will use to run the experiments
>|| trader categories are taken from Kirilenko et al
>|| numbers of each category of trader are also taken from Kirilenko et al
>||
>
>sim:: sentiment -> [sys_message]
>sim sent 
>    = tracer lobs traces
>      where
>      res = startlob : lobs
>      startlob = lob_setsentiment sent emptylob
>      (lobs,traces)      = (simstep startlob (makers ++ takers ++ intermediaries ++ hfts ++ fundamentalsellers ++ fundamentalbuyers ++ smalltraders ++ opportunistics))
>      intermediaries     = create_traderlist num_interm generic_trader Intermediary randoms gs res  || Kirilenko says 11
>      hfts               = create_traderlist num_hft generic_trader HFT randoms gs res   || Kirilenko says 1 but generates hd of [] error!!!!
>      fundamentalbuyers  = create_traderlist num_fundbuyer generic_trader FundBuyer randoms gs res  || 2 Kirilenko says 79
>      fundamentalsellers = create_traderlist num_fundseller generic_trader FundSeller randoms gs res  || 2 Kirilenko says 80
>      smalltraders       = create_traderlist num_small generic_trader Small randoms gs res  || 3 Kirilenko says 430
>      opportunistics     = create_traderlist num_opp generic_trader Opportunistic randoms gs res   || 3 Kirilenko says 363
>      makers             = create_traderlist num_ssm generic_trader    SSMaker randoms gs res   
>                           ++ create_traderlist num_bsm generic_trader BSMaker randoms gs res
>      takers             = create_traderlist num_bst generic_trader    BSTaker randoms gs res   
>                           ++ create_traderlist num_sst generic_trader SSTaker randoms gs res
>      randoms = rands 10
>      gs = gaussians 10
>
>create_traderlist :: num -> (traderid ->[num]->[num]->[lob]->traderstate->[order]) -> (num->traderid) -> [num] ->[num] -> [lob]-> [[order]]
>create_traderlist 0 f cat rs gs res = []
>create_traderlist n f cat rs gs res = ((f (cat n) rs gs res emptytraderstate) : (create_traderlist (n-1) f cat rs gs res))

>tracer :: [lob] -> [[char]] -> [sys_message]
>tracer []     []       = (Tofile "trace" "END OF TEST\n"):[Exit 0]
>tracer []     (tr:trs) = (Tofile "trace" "END OF TEST\n"):[Exit 0]
>tracer (x:xs) []       = (Tofile "trace" "END OF TEST\n"):[Exit 0]
>tracer (x:xs) (tr:trs) = [Tofile "trace" ((show x) ++ "\n================================\n")] ++ 
>                         [Tofile "data.csv" tr] ++
>                         (tracer xs trs)

>simstep :: lob -> [ [order] ] -> ([lob], [[char]])
>simstep ob [] = ([],[])
>simstep ob xs = ([],[]), if (member xs [])
>simstep ob xs = ((ob1 : morelobs), (tr : moretraces))
>                where
>                (morelobs, moretraces) = simstep ob1 (map safetl xs)
>                (ob1,tr) = foldr g (ob,[]) (map safehd xs)
>                           where
>                           g ord (book,trace) = (match_m3 ord book, trace++(lob_gettrace book))
>                safehd [] = error "tried to take head of empty list"
>                safehd (x:xs) = x
>                safetl [] = error "tried to take tail of empty list"
>                safetl (x:xs) = xs



