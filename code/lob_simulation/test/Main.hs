import Test.QuickCheck
import Test.Framework (defaultMain)
import Simulation.Properties.MultiLevelList

main = defaultMain tests

tests = [multiLevelListTestGroup]
