
module Main where

import           Test.Prelude hiding (evaluate, exp)

import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import           Plate
import qualified PlateExamples as PE
import           Test.Tasty
import           Test.Tasty.HUnit hiding (assert)
import           Test.Tasty.QuickCheck

import qualified Test.ReadmeExamples as ReadmeExamples
import           Test.SimplePlate () -- Just make sure it compiles.
import           Test.Validation

main :: IO ()
main = do
  writeLibraryExamples
  ReadmeExamples.write
  writeTests "./test/generated/concrete.json" concreteTests
  writeTests "./test/generated/expressions.json" expressionTests
  defaultMain $ testGroup "Plate"
    [ testGroup "Plate representations" plateTests
    , testGroup "JSON representations" jsonTests
    , testGroup "Concrete" (treeFromCase <$> concreteTests)
    , testGroup "Expressions" (treeFromCase <$> expressionTests)
    , testGroup "The schema for schemas" (pure testSchema)
    ]

plateTests :: [TestTree]
plateTests =
  [ testProperty "Plate/Plate isomorphism"
      (isomorphicPlate :: Plate -> Bool)
  , testProperty "Schema/Plate isomorphism"
      (isomorphicPlate :: Schema -> Bool)
  , testProperty "Expression/Plate isomorphism"
      (isomorphicPlate :: Expression -> Bool)
  ]

jsonTests :: [TestTree]
jsonTests =
  [ testProperty "Plate/JSON isomorphism"
      (isomorphicJSON :: Plate -> Bool)
  , testProperty "Schema/JSON isomorphism"
      (isomorphicJSON :: Schema -> Bool)
  , testProperty "Expression/JSON isomorphism"
      (isomorphicJSON :: Expression -> Bool)
  , testCase "Special case for bool"
      (toJSON (PPrimitive (HM.singleton "bool" (PString "true")))
        @?= Bool True)
  , testCase "Special case for maybe"
      (toJSON (PPrimitive (HM.singleton "nothing" (PDictionary mempty)))
        @?= Null)
  , testCase "Special case for dictionaries"
      (toJSON (PDictionary (HM.singleton (PInteger 1) (PString "foo")))
        @?= Object (HM.singleton "dictionary"
                                 (Array (V.fromList
                                   [ Array (V.fromList [ Number 1
                                                       , String "foo"
                                                       ])
                                   ]))))
  ]

isomorphicPlate :: (Eq a, ToPlate a, FromPlate a) => a -> Bool
isomorphicPlate a = fromPlate (toPlate a) == Right a

isomorphicJSON :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
isomorphicJSON a = fromJSON (toJSON a) == Aeson.Success a

treeFromCase :: TestCase Expression -> TestTree
treeFromCase a =
  testGroup (show (_tgType a))
    [ testCase
        "Valid assertions"
        (traverse_ (validCase (_tgType a)) (_tgValid a))
    , testCase
        "Invalid assertions"
        (traverse_ (invalidCase (_tgType a)) (_tgInvalid a))
    ]

validCase :: Expression -> Value -> Assertion
validCase a b = do
  let Aeson.Success plate = fromJSON b
  validate mempty a plate @?= Right ()

invalidCase :: Expression -> Value -> Assertion
invalidCase a b = do
  let Aeson.Success plate = fromJSON b
  case validate mempty a plate of
    Left _   -> pure ()
    Right () -> assertFailure "An invalid value slipped through"

testSchema :: TestTree
testSchema =
  testProperty "Validates generated expressions" f
  where
    f :: Expression -> Bool
    f exp = validate (HM.fromList PE.exampleLibrary)
                     (Builtin PE.expression)
                     (toPlate exp) == Right ()

writeLibraryExamples :: IO ()
writeLibraryExamples =
  BS.writeFile "./test/generated/examples.json" (encodePretty examples <> "\n")
  where
    examples :: [(Text, Value)]
    examples = fmap toJSON <$> PE.exampleLibrary

writeTests :: Text -> [TestCase Expression] -> IO ()
writeTests name tests =
  BS.writeFile (T.unpack name) (encodePretty (toJSON <$> tests) <> "\n")
