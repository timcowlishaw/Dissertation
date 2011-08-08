module ABSSketch.Util where
  justIf pred o |    pred o = Just o
                | otherwise = Nothing
