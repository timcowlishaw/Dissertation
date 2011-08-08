module Simulation.Agents.Domain.Agent where
  import Remote.Encoding

  --Agent should have state? (Macal and North 2010)

  class Agent a where
    initialize :: ProcessM ()
    work :: ProcessM ()
    instruments :: [ProcessM ()]
