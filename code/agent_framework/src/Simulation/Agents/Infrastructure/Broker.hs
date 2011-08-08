import Remote
import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map

type SubscriberList = Map ChannelId [ProcessId]
type Broker = StateT SubscriberList ProcessM 

brokerProcess :: Broker () 
brokerProcess = do
  receiveWait [
    match (\(Subscribe channel pid) -> ),
    match (\(Unsubscribe channel pid) -> ),
    match (\(Send channel pid) -> )
  ] >> brokerProcess


