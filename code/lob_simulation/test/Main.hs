import Test.QuickCheck
import Test.Framework (defaultMain)
import Simulation.Tests.LimitOrderList
import Simulation.Tests.LimitOrderBook
main = defaultMain tests

tests = [limitOrderListTests, limitOrderBookTests]
