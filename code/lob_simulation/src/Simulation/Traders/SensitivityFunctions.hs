module Simulation.Traders.SensitivityFunctions where
  import Simulation.Types  
  import Simulation.Utils

  lowImbalanceSensitivity :: ImbalanceFunction
  lowImbalanceSensitivity = const 1
  
  mediumImbalanceSensitivity :: ImbalanceFunction
  mediumImbalanceSensitivity = (0.5-) . sigmoid . fromIntegral

  highImbalanceSensitivity :: ImbalanceFunction
  highImbalanceSensitivity = min 40 . exp . fromIntegral

  lowVolatilitySensitivity :: VolatilityFunction
  lowVolatilitySensitivity = const 1

  mediumVolatilitySensitivity :: VolatilityFunction
  mediumVolatilitySensitivity = (0.5-) . sigmoid

  highVolatilitySensitivity :: VolatilityFunction
  highVolatilitySensitivity = min 40 . exp  
