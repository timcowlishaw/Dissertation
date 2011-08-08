module Simulation.Properties.MultiLevelList (multiLevelListTestGroup) where
  import Test.Framework (testGroup)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.QuickCheck
  import Simulation.Generators
  import Simulation.MultiLevelList
  import Simulation.Order

  multiLevelListTestGroup = testGroup "Multi-level list" [
      testProperty "emptyList is the identity for delete"   prop_emptyListIsIdentityForDelete,
      testProperty "insert is the right inverse of delete"  prop_insertIsRightInverseOfDelete,
      testProperty "insert is associative"                  prop_insertIsAssociative,
      testProperty "delete is idempotent"                   prop_testDeleteIsIdempotent 
    ]
 

  prop_emptyListIsIdentityForDelete :: Order Buy Limit -> Bool
  prop_emptyListIsIdentityForDelete order = delete' empty == empty
    where delete' = delete order

  prop_insertIsRightInverseOfDelete :: Order Buy Limit -> MultiLevelList (Order Buy Limit) -> Bool
  prop_insertIsRightInverseOfDelete order orderList = (delete' . insert' $ orderList) == orderList
    where insert' = insert order
          delete' = delete order

  prop_insertIsAssociative :: Order Buy Limit -> Order Buy Limit -> MultiLevelList (Order Buy Limit) -> Bool
  prop_insertIsAssociative order1 order2 orderList = (insert1 . insert2 $ orderList) == (insert2 . insert1 $ orderList)
    where insert1 = insert order1
          insert2 = insert order2

  prop_testDeleteIsIdempotent :: Order Buy Limit -> MultiLevelList (Order Buy Limit) -> Bool
  prop_testDeleteIsIdempotent order orderList = (delete' . delete' $ list) == (delete' list)
    where delete' = delete order
          list    = insert order orderList
