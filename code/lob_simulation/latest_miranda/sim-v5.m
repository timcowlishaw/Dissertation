>|| This is a literate script - VERSION 4 - MODIFICATION 1
>|| Copyright Christopher D. Clack 2011
>|| Attenuation of takers
>%include "./lib-localcopy/sim-orderlist.m"
>%include "./lib-localcopy/sim-lob.m"



=====================================
TRADERS

Traders are characterised by 
(i)  their target inventory level; 
(ii) their increased demand for or supply of securities at each simulated time step; and
(iii) four functions that determine their trading behaviour – 
	(a) according to the gap between current price and current underlying value; 
	(b) according to the gap between current inventory and target inventory; 
	(c) in response to order imbalance; and 
	(d) in response to sudden changes in price:

Thus we use the tuple (target, demand, supply, pricefunction, inventoryfunction, orderfunction, volatilityfunction).  
The trader categories are:
	Fundamental Buyer          - target inventory=20,000; demand=200; supply=0; pf; invfun; oflow;  vfhigh
	Fundamental Seller         - target inventory=20,000; demand=0;   supply=0; pf; invfun; oflow;  vfhigh
	Intermediary (incudes HFT) - target inventory=2,000;  demand=0;   supply=0; pf; invfun; ofhigh; vflow
	Opportunistic              - target inventory=1,000;  demand=0;   supply=0; pf; invfun; ofmed;  vfmed
	Small                      - target inventory=500;    demand=0;   supply=0; pf; invfun; ofmed;  vfmed

>target_inventory (FundBuyer     id) = 20000
>target_inventory (FundSeller    id) = 100
>target_inventory (Intermediary  id) = 1200
>target_inventory (HFT           id) = 800
>target_inventory (Opportunistic id) = 800
>target_inventory (Small         id) = 400

>demand (FundBuyer     id) = 200
>demand (FundSeller    id) = 0
>demand (Intermediary  id) = 0
>demand (HFT           id) = 0
>demand (Opportunistic id) = 0
>demand (Small         id) = 0

>supply (FundBuyer     id) = 0
>supply (FundSeller    id) = 200
>supply (Intermediary  id) = 0
>supply (HFT           id) = 0
>supply (Opportunistic id) = 0
>supply (Small         id) = 0

>tradersensitivity (FundBuyer     id) = (oflow, vfhigh)
>tradersensitivity (FundSeller    id) = (oflow, vfhigh)
>tradersensitivity (Intermediary  id) = (ofhigh, vflow)
>tradersensitivity (HFT           id) = (ofhigh, vflow)
>tradersensitivity (Opportunistic id) = (ofmed, vfhigh)
>tradersensitivity (Small         id) = (ofmed, vfhigh)

>|| Support functions
>tangent x = (sin y) / (cos y)
>            where
>            y = (sigmoid x) * pi
>sigmoid x = (1 / (1 + exp (-x))) - 0.5  || gives output between +0.5 and -0.5

>|| oflow, ofmed and ofhigh provide low, medium and high sensitivity to order imbalance
>|| They take in a number which is the amount of order flow imbalance and they return a multiplier for order size
>oflow x = 1
>ofmed x =  1-(sigmoid x)
>ofhigh x = min [exp x, 40]

>|| vflow, vfmed and vfhigh provide low, medium and high sensitivity to volatility (sudden price change)
>|| They take in a number which is the change in traded prices since the last timestep and they return a multiplier for order size
>vflow x = 1
>vfmed x = 1-(sigmoid x)
>vfhigh x = min [exp x, 40]

>|| pf is the pricing function that ensures traders place orders just ahead of top of book if there is high TOB depth, and behind
>|| top of book if there is low TOB depth.
>|| pf takes the depth at the top of the book and returns a fraction (must be applied to spread to give offset from TOB price)
>|| We assume depth >= 0 at all times and anything over depth=2000 is saturated
>|| The output is bounded by -0.25 and +0.25
>pf depth = max [ min [0.25,  curve], -0.25 ]
>           where
>           depth1 = min [depth, 2000]
>           curve = 0.25*(tangent (((2*depth1)-2000)/2000))/1.56

>|| INVFUN
>|| ======
>|| The function invfun does two things - it calculates a revised target (based on fundamental target and current price and current value); 
>|| and it then calculates an amount to buy or sell based on the revised target and the current inventory.
>|| The inputs are the catid, the curent value, the current midprice, and the current inventory.  The function might also take into account
>|| the spread (but doesn't do so yet!).  This response function is different for each category of trader.
>|| In general terms, if inv < target then buy (+ve amount) else sell (-ve amount)
>||
>|| Worked example for FundBuyer:
>|| x = (value - price)                ------  Thus, if x is positive we are encouraged to buy because the underlying value is higher than the midprice
>|| (sigmoid x)                        ------  gives an output between +0.5 (if x is positive) and -0.5 (if x is negative)
>|| 1 + (sigmoid x)                    ------  creates a multiplier to give increased buying (if x positive) or selling (if x negative) pressure
>|| target_inventory * (1 + sigmoid x) ------  changes the target - an increased target (positive x) means greater pressure to buy
>|| y = inv - changed target           ------  the difference between current inventory and target inventory; buy if y is negative
>|| targetsize                         ------  the target size of a single order
>|| amount = targetsize - (exp(y/150)) ------  if y negative, (exp(y/150)) is very small so amount is positive and close to target size
>||                                            if y positive and (exp(y/150))>targetsize then amount is negative - need to sell
>||                                            the number 150 is empirically determined
>|| min [amount, 2000]                 ------  upperbound of buy size is 2,000
>|| max [amount, -2000]                ------  lowerbound of buy size is -2,000
>||
>invfun (FundBuyer id) value price inv     = min [amount, 2000], if  amount > 0
>                                          = max [amount, -2000], otherwise
>                                            where
>                                            amount = targetsize - (exp(y/150))
>                                            targetsize = 200
>                                            y = inv-((target_inventory (FundBuyer id))*(1+sigmoid x))
>                                            x = (value - price)
>invfun (FundSeller id) value price inv    = min [amount, 2000], if amount > 0
>                                          = max [amount, -2000], otherwise
>                                            where
>                                            amount = (exp(-y/150)) - targetsize  
>                                            targetsize = 200
>                                            y = inv-((target_inventory (FundSeller id))*(1+sigmoid x))
>                                            x = (value - price)
>invfun (Intermediary id) value price inv  = min [amount, 2000], if amount > 0
>                                          = max [amount, -2000], otherwise
>                                            where
>                                            amount = 0.2 * (-(tangent (y/700)))*targetsize    || titrate the parameters for Intermediary!
>                                            targetsize = 100
>                                            y = inv-((target_inventory (Intermediary id))*(1+sigmoid x))
>                                            x = (value - price)
>invfun (HFT          id) value price inv  = min [amount, 2000], if amount > 0
>                                          = max [amount, -2000], otherwise
>                                            where
>                                            amount = 0.2 * (-(tangent (y/700)))*targetsize    || titrate the parameters for HFT!
>                                            targetsize = 100
>                                            y = inv-((target_inventory (HFT id))*(1+sigmoid x))
>                                            x = (value - price)
>invfun (Opportunistic id) value price inv = min [amount, 250], if amount > 0
>                                          = max [amount, -250], otherwise
>                                            where
>                                            amount = f ((-(tangent (y/700)))*targetsize)    || titrate the parameters for Opportunistic!
>                                            targetsize = 50
>                                            y = inv-((target_inventory (Opportunistic id))*(1+sigmoid x))
>                                            x = (value - price)
>                                            f x = x
>||                                          f x = abs x, if (value > price)
>||                                              = - (abs x), otherwise
>invfun (Small        id) value price inv  = min [amount, 250], if amount > 0
>                                          = max [amount, -250], otherwise
>                                            where
>                                            amount = f (((-1)^(id mod 2)) * 0.9 * (-(tangent (y/700)))*targetsize)    || titrate the parameters for Small!
>                                            targetsize = 100
>                                            y = inv-((target_inventory (Small id))*(1+sigmoid x))
>                                            x = (value - price)
>                                            f x = x
>||                                          f x = abs x, if (value > price)
>||                                              = - (abs x), otherwise

>foo = [(0.2 * (-(tangent (y/700)))*100) | y <- [-1000,-800,-600,-400,-200,0,200,400,600,800,1000]]
>|| unit testing of invfun
> invfun_ut catid = map (entier . (invfun catid 5 10)) [t - 1000, t - 900 .. t+1000]
>                   where
>                   t = target_inventory catid

>traderstate == (num, num, num)
>emptytraderstate = (0, 0, 0)

>|| A generic trader takes as arguments:
>||    - a catid comprising a constructor giving the category of the trader and an ID number
>||    - a list of random numbers
>||    - a list of numbers with gaussian distribution 
>||    - a list of time-ordered snapshots of the limit order book - only look at the head of this list!
>||    - a traderstate holding state information only of interest to this instance of the trader
>|| And the returned result is a list of orders to be sent to the exchange
>||

>generic_trader :: traderid -> [num] -> [num] -> [lob] -> traderstate -> [order]
>generic_trader catid (r:rs) (g:gs) [] state = []
>generic_trader catid [] (g:gs) (x:xs) state = error "generic trader: run out of random numbers"
>generic_trader catid (r:rs) [] (x:xs) state = error "generic trader: run out of gaussian numbers"
>generic_trader catid (r:rs) (g:gs) (x:xs) (old_bestbid, old_bestoffer, old_ordernum)
>    = [], if time > 100 || 980
>    = (d2 catid sent) : (generic_trader catid rs gs xs newstate), otherwise
>||    = (d1 catid sent) : (generic_trader catid rs gs xs newstate), otherwise
>      where
>      newstate = (bestbid, bestoffer, ordernum)
>      ordernum = lob_getordernum x
>      ordernum_move = (ordernum - old_ordernum) / old_ordernum, if (old_ordernum ~= 0)
>                    = 0.05, otherwise
>      time = lob_gettime x
>      bestbid = lob_getbestbid x
>      bestoffer = lob_getbestoffer x
>      midprice = (bestbid + bestoffer)/2
>      spread = bestoffer - bestbid
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
>                     getmovement o = osize,     if (or [otype = Bid, otype = Buy])
>                                   = (- osize), if (or [otype = Sell, otype = Offer])
>                                   = 0, otherwise
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
>      (ordertype, orderprice, ordersize) = (ot, (entier (abs op)), (entier (abs os)))
>                                           where
>                                           newinv = my_inventory + (((supply catid) - (demand catid))*time)
>                                           os = (vf pricechange) * ((of imbalance) * (invfun catid current_value midprice newinv)) 
>                                           pricechange = 2*(bidpricemove + offerpricemove)
>                                           imbalance = (lob_getsellsidedepthneartop x) - (lob_getbuysidedepthneartop x)
>                                           ot = Sell, if or [and [(os < 0), os_is_large], and[(os < 0), is_smalltrader catid]]
>                                              = Offer, if (os < 0)
>                                              = Buy, if or [and [(os >= 0), os_is_large], and [(os >= 0), is_smalltrader catid]]
>                                              = Bid, otherwise
>                                           || Price Logic - compare size (os), bestbid (b), bestoffer (bo) and current value (cv)
>                                           || Assume large size implies greater need to trade
>                                           || If (bb > bo > cv), crossed book + falling; Smallbid at cv,  Largebid at bo;  Smalloffer at bb,  Largeoffer at bo
>                                           || If (bb > cv > bo), crossed book + stable;  Smallbid at bo,  Largebid at bo;  Smalloffer at bb,  Largeoffer at bb
>                                           || If (cv > bb > bo), crossed book + rising;  Smallbid at bo,  Largebid at bb;  Smalloffer at cv,  Largeoffer at cv
>                                           || If (bo > bb > cv),                falling; Smallbid at cv,  Largebid at bo; Smalloffer at bb,  Largeoffer at cv+
>                                           || If (bo > cv > bb),                stable;  Smallbid at bb+, Largebid at cv-; Smalloffer at bo-, Largeoffer at bb
>                                           || If (cv > bo > bb),                rising;  Smallbid at bo,  Largebid at bb+;  Smalloffer at cv,  Largeoffer at bo-
>                                           op = opBb, if and[ot=Bid, os_is_large]
>                                              = opBs, if (ot=Bid)
>                                              = opOb, if and[ot=Offer, os_is_large]
>                                              = opOs, otherwise
>                                           opBs = bestoffer,                           if and [cv >= bestoffer, bestoffer ~=0]
>                                                = bestbid + (a1 * (cv-bestbid)),       if (bestoffer >= cv >= bestbid)
>                                                = cv,                                  if (bestbid >= bestoffer >= cv)
>                                                = cv,                                  if (bestoffer >= bestbid >= cv)   || bestbid
>                                                = cv,                                  if (bestoffer=0)
>||                                              = cv - (abs (a1 * (bestbid - cv))),    otherwise   || not sure if we need this
>                                                = error ("opBs: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)
>                                                           ++" a2="++(show a2)++"\n"), otherwise 
>                                           opBb = bestoffer,                           if and [(bestbid >= bestoffer >= cv), bestoffer ~= 0]
>                                                = bestoffer,                           if and [(bestbid >= cv >= bestoffer), bestoffer ~= 0]
>                                                = bestbid,                             if and [(cv >= bestbid >= bestoffer), bestbid ~= 0]
>                                                = bestoffer,                           if and [(cv >= bestoffer >= bestbid), bestoffer ~= 0]
>                                                = cv - (abs (a1 * (bestbid - cv))),    if (bestoffer >= bestbid >= cv)
>                                                = bestbid + (a1 * (cv-bestbid)),       if (bestoffer >= cv >= bestbid)
>                                                = cv - (abs (a1 * (bestbid - cv))),    otherwise   || not sure if we need this
>                                           opOs = bestbid,                             if (bestbid >= bestoffer >= cv)
>                                                = bestbid,                             if (bestbid >= cv >= bestoffer)
>                                                = cv,                                  if and [(cv >= bestbid >= bestoffer), bestbid ~= 0] || bestbid
>                                                = cv,                                  if and [(cv >= bestoffer >= bestbid), bestoffer ~= 0] || ****************
>                                                = bestbid,                             if (bestoffer >= bestbid >= cv)
>                                                = bestoffer - (a2 * spread),           if (bestoffer >= cv >= bestbid) || made it work if 30* 
>                                                = cv + (abs (a2 * (cv-bestoffer))),    otherwise   
>||                                                = error ("opOs: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)
>||                                                         ++" a2="++(show a2)++"\n"), otherwise 
>                                           opOb = bestoffer,                           if (bestbid >= bestoffer >= cv)
>                                                = bestbid,                             if (bestbid >= cv >= bestoffer)
>                                                = cv,                                  if (cv >= bestbid >= bestoffer)
>                                                = cv + (abs (a2 * (cv-bestbid))),      if (cv >= bestoffer >= bestbid)  || **************
>                                                = bestbid,                             if (bestoffer >= bestbid >= cv)
>                                                = bestoffer - (a2 * spread),           if (bestoffer >= cv >= bestbid)
>                                                = error ("opOb: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)++"\n"), otherwise 
>||                                                = cv + (abs (a2 * (cv-bestoffer))),    otherwise   || not sure if we need this
>                                           os_is_large = (abs(os) > 3000)
>                                           is_smalltrader (Small x) = True
>                                           is_smalltrader any       = False
>                                           a1 = 0.5 * (pf (lob_getbuysidedepthneartop x))
>                                           a2 = 0.5 * (pf (lob_getsellsidedepthneartop x))
>                                           cv = current_value
>                                           (of, vf) = tradersensitivity catid
>|| New version replaces d1
>      d2 identifier sent = order_create orderprice ordersize time catid ordertype
>
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




>|| sim is the simulator we will use to run the experiments
>|| trader categories are taken from Kirilenko et al
>|| numbers of each category of trader are also taken from Kirilenko et al
>||
>
>sim:: sentiment -> num -> [sys_message]
>sim sent matcher
>    = tracer lobs traces
>      where
>      res = startlob : lobs
>      startlob = lob_setsentiment sent primed_emptylob
>      (lobs,traces)      = (simstep matcher startlob (makers ++ takers ++ intermediaries ++ hfts ++ fundamentalbuyers ++ fundamentalsellers ++ smalltraders ++ opportunistics))
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

>simstep :: num -> lob -> [ [order] ] -> ([lob], [[char]])
>simstep m ob [] = ([],[])
>simstep m ob xs = ([],[]), if (member xs [])
>simstep m ob xs = ((ob1 : morelobs), (tr : moretraces))
>                  where
>                  (morelobs, moretraces) = simstep m (lob_increment_time ob1) (map safetl xs)
>                  (ob1,tr) = foldr g (ob,[]) (map safehd xs)
>                             where
>                             g ord (book,trace) = (matchfun ord book, trace++(lob_gettrace book))
>                  safehd [] = error "tried to take head of empty list"
>                  safehd (x:xs) = x
>                  safetl [] = error "tried to take tail of empty list"
>                  safetl (x:xs) = xs
>                  matchfun = match, if m = 0
>                           = match_m4 (20,150,1,0.02,0.05), otherwise
>


