class (Arrow (~>)) => ArrowPair (~>) where
  fst :: (a,b) ~> a
  snd :: (a, b) ~> b 
  swap :: (a, b) ~> (b, a)
  lassoc :: (a,(b,c)) ~> ((a,b),c)
  rassoc :: ((a, b), c) ~> (a, (b, c))
