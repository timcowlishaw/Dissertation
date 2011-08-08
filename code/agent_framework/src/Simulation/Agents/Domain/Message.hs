package Simulation.Agents.Domain.Message where

  --Message should really be a class to afford pattern-matching on constructors. How will this work with the Pubsub queue? How do we pass a reference to a channel name

  data Message = { channel :: (Channel a) => a, id :: String, payload :: (Serializable a) => a }

  instance Binary Message where
    get = 
    put

  instance Typeable Message where
    
  
