import           Common
import           Source

import           Test.Tasty               (defaultMain, testGroup)
import           Test.Tasty.HUnit         (assertEqual, assertFailure, testCase)

import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

main = defaultMain allTests

allTests = testGroup "All tests" [sourceTests]

sourceTests = testGroup "Source tests" [createSourceTest, updateSourceTest]

createSourceTest =
  testCase "Create source" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=Lisere77"
    newSourceId <- createSource "Title" c
    source <- readSource c newSourceId
    assertEqual "Source Id equal" newSourceId (Common.getSourceId source)
    assertEqual "Source Title equal" "Title" (Common.getSourceTitle source)
    removed <- deleteSource newSourceId c
    assertEqual "Source removed" True removed

updateSourceTest =
  testCase "Update source" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=Lisere77"
    newSourceId <- createSource "Title" c
    source <- readSource c newSourceId
    assertEqual "Source Id equal" newSourceId (Common.getSourceId source)
    assertEqual "Source Title equal" "Title" (Common.getSourceTitle source)
    source <- updateSource newSourceId "Title1" c
    assertEqual "Source Id equal" newSourceId (Common.getSourceId source)
    assertEqual "Source Title equal" "Title1" (Common.getSourceTitle source)
    removed <- deleteSource newSourceId c
    assertEqual "Source removed" True removed