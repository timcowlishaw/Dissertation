lsim-orderlist.m N½  !num_ssm    num_bsm   num_bst   num_sst  num_opp  num_small  num_fundseller  num_fundbuyer  num_hft  num_interm sin µ?¹        ·pi         sines  sines ¨Calm     Ðÿÿÿÿ¨Choppy  °¶°·°¶©sines ?ð        Üÿÿÿÿ  èÿÿÿÿ¨Ramp  ©µ  Ìÿÿÿÿ  Ðÿÿÿÿ  Ðÿÿÿÿ¨Toxic  °»(  Ðÿÿÿÿê value sentiment  value gaussians µfoldr ¶ map ¯°¹©rands   èÿÿÿÿ gaussians rands °º¯ºentier °¹°¶°·abs   Nm     ÿÿÿÿ  09ÿÿÿÿ       ÿÿÿÿ       ÿÿÿÿ  èÿÿÿÿ ü rands ðToxic Ramp Choppy Calm    
 ó sentiment Calm   sentiment  ó Calm Choppy  sentiment  ó Choppy Ramp  sentiment  ó Ramp Toxic  sentiment  ó Toxic ðTaker Maker SSMaker BSMaker BSTaker SSTaker Opportunistic Small FundBuyer FundSeller HFT Intermediary Phantom    
 ñ traderid Phantom   traderid  ñ Phantom Intermediary  traderid  ñ Intermediary HFT  traderid  ñ HFT FundSeller  traderid  ñ FundSeller FundBuyer  traderid  ñ FundBuyer Small  traderid  ñ Small Opportunistic  traderid  ñ Opportunistic SSTaker  traderid  ò SSTaker BSTaker  traderid  ò BSTaker BSMaker 	 traderid  ò BSMaker SSMaker 
 traderid  ò SSMaker Maker  traderid  ò Maker Taker  traderid  ò Taker ðAbort None Buy Sell Offer Bid     
 ð ordertype Bid   ordertype  ð Bid Offer  ordertype  ð Offer Sell  ordertype  ð Sell Buy  ordertype  ð Buy None  ordertype  ð None Abort  ordertype  ð Abort   
 ï time   
 î size   
 í price traderid ordertype showorder  
 Ó order °¯¯¯¯ð$(¯¯Îð ,¯°¯¯¯Îð ta ,serahs ¯¯Îð morf  ,sces ¯¯  ð epyt fo ,°   ð     
)order  æ showorder ¨¯¯¯¯¯¦°°¯¯¯¯¦¯¦°¯¦¦ïîorder order  â order_equals ¯¯¯¯°¯®¼ ­¯°¯¬¬«¯error ð ecirp evitagen ot redro gnitadpUÎorder order  à order_newprice ¯¯¯¯¯¯®¼ ¯°¯°¯¬­¬«¯error ð ezis evitagen ot redro gnitadpUÎorder order  Þ order_newsize ¯¯order  Û order_getsize order traderid  Ý order_gettraderid order ordertype  Ü order_gettype ¯order  Ú order_gettime ¯°¯¯¯¬¯°¯¬¬«order order  Ù order_setprice ¯¯¯order  Ø order_getprice °¯¬¯°¯¬¬«traderid ordertype order  × order_create ¨§ § § ïîorder  Õ isemptyorder None Phantom    order  Ô emptyorder order showorderlist  
 ! orderlist ¨¦ððLODNE¨°¯¯Îð   :¯¯óhowlist showorder ð
showorderlist ê ¸ showorderlist orderlist  ¸ showorderlist ¨¦ð¯¬emptyorderlist «¨¦ðBuy  °ol_fill_cross Buy ¨¯Buy  °°°°®°®°°°°°°°°®°°°®°°°°°°°°°°°°®®°°°°°®°°°®®°®°°°°°°®®°°®°°®®°°®°¯¯¯¯¯¯¯¯¯¯¯°°°¯¯¯®¯¯¯°°°»¯¯¯°°°°¯¯¯¯¯¬«¬««®°®®°°°®°®®°°®°®®°°¯®¯¯¯¯°°°¯¯¯¯¯isemptyorder ¯°°°¯¯°¬«¬««emptyorder °¯¯¯¯¯°¬«««emptyorder ¯°°¯¯¯¯»°°°°¬°¯­¬­«emptyorder ¯°¯¯ol_fill_cross ««Buy °¯°­¯¯order_newprice order_newprice order_newsize °¯°­¯¯order_newprice order_newsize order_newprice ¯order_newsize µ¯order_newsize µ°°¹¶order_getsize order_getsize order_getprice order_getprice ¨Sell  ¯ol_fill_cross ol_reverse Buy error ðdedivorp ton reffo - redro dessorc llif tonnaCorderlist order ordertype order order orderlist order order order   ol_fill_cross ¨¦ð¯¬emptyorderlist «¨¦ðBuy  °ol_fill Buy ¨¯Buy  °°°°®°®°°°°°°°°®°®°®°°®®°®°®°°°°®®°°®°°®®®°°°¯¯®®¯¯°isemptyorder ¯¯¯¯°°°¯¯°¬«¬««emptyorder ¯®®®¯¯°°¯¯¯°¬«««emptyorder ®®®°¯¯°°°¯¯°°»°°°°¬°¯­¬­«emptyorder ¯¯°¯¯ol_fill ««Buy ¯¯­­order_newsize order_newprice order_getprice ¯order_newsize µ®¯­¬°°order_newprice order_newsize order_getprice ¯order_newsize µorder_getsize order_getsize ¨Sell  ¯ol_fill ol_reverse Buy error ðdedivorp ton redro tekram - redro llif tonnaCorderlist order ordertype order order orderlist order order order   ol_fill °¯filter ðsnd ð¯¨¦ð¯¯¨°°ê x isort ¨¦ð­ð¯¨¯¯®¯¯®»°°¯¯¯¬«««°¯°°¬««ê z insert orderlist orderlist  v ol_clean ¨¦ðerror ðsredro fo tsil ytpme na morf redro tsewol pop t'naC¨¦ðol_poplowest ¨¯°¯¬««ê r ol_poplowest orderlist order orderlist  r ol_poplowest ¨¦ð ¨Offer  ¯°°°¯°°¯°¯¯¯¶¯¨¦ð ¯¯¯¨¯°¯¯°°°°®°°°°°¯¯¯¯¯»·¶¶ ê l f ¨¦ð ¯¨¯¶order_getsize ê j sumsizes ¨Bid   °ol_getdepthneartop Offer reverse ê g ol_getdepthneartop ordertype orderlist  g ol_getdepthneartop ¨¦ð ¨Offer  ¨¦ð ¯¨¯¶order_getsize ê [ sumsizes ¨Bid   ¦ð¨¦ð ¯¨¯¶order_getsize ê _ sumsizes ¨Bid   ol_getdepth Bid ê X ol_getdepth ordertype orderlist  X ol_getdepth ¨¦ð Àorderlist  R ol_getlevels ¨¦ð ¨°¯¶foldr ¶ map order_getsize ol_getliquidity ê O ol_getliquidity orderlist  O ol_getliquidity ¨¦ðerror ðsredro fo tsil ytpme ni ecirp tsewol teg t'naC¨ê L ol_getlowestprice orderlist  L ol_getlowestprice ¨¦ðerror ðsredro fo tsil ytpme ni ecirp tsehgih teg t'naC¨¦ð¨ol_gethighestprice ê H ol_gethighestprice orderlist  H ol_gethighestprice ¨¦ðð¨°ol_reverse ­ðê µ ol_reverse orderlist orderlist  µ ol_reverse ¨¦ðð¨¯¦°¯¯¯¯°­«°¨¦ðð°¨¯¯¯order_equals ¬ê @ delete ¨¯®®®°°°¯¯¯¯»««¯¯¬«ol_delete ê ; ol_delete orderlist order orderlist  ; ol_delete ¨¦ð°­¬­ðð¨¯¦°¯¯¯¯°­«°¨¦ð­ð°¨¯®®¯¯»order_gettime order_gettime °¬«¬ê 3 insert ¨¯®®®°°°¯¯¯¯»­¬­ð««¯¯¬«ol_insert ê . ol_insert orderlist order orderlist  . ol_insert ¨¦ðð ¨¦ð¨ol_last ê ' ol_last orderlist order  ' ol_last ¨¦ðð ¨ê $ ol_first orderlist order  $ ol_first ¨¦ðïîorderlist  + isemptyorderlist ðorderlist  " emptyorderlist  ¨×Toxic  ¨×Ramp  ¨×Choppy  ×Calm    ¨×Abort  ¨×None  ¨×Buy  ¨×Sell  ¨×Offer  ×Bid     ¨×Taker  óhownum1 ¨×Maker  óhownum1 ¨×SSMaker 
 óhownum1 ¨×BSMaker 	 óhownum1 ¨×BSTaker  óhownum1 ¨×SSTaker  óhownum1 ¨×Opportunistic  óhownum1 ¨×Small  óhownum1 ¨×FundBuyer  óhownum1 ¨×FundSeller  óhownum1 ¨×HFT  óhownum1 ¨×Intermediary  óhownum1 ×Phantom    ððð