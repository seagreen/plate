
module Test.Validation where

import           Protolude

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Plate

data TestCase a = TestCase
  { _tgType    :: a
  , _tgValid   :: [Value]
  , _tgInvalid :: [Value]
  } deriving (Eq, Show, Functor)

instance ToJSON a => ToJSON (TestCase a) where
  toJSON a = object
    [ "type"    .= _tgType a
    , "valid"   .= _tgValid a
    , "invalid" .= _tgInvalid a
    ]

instance FromJSON a => FromJSON (TestCase a) where
  parseJSON = withObject "TestCase" $ \o -> TestCase
    <$> o .: "type"
    <*> o .: "valid"
    <*> o .: "invalid"

concreteTests :: [TestCase Expression]
concreteTests =
  [ Builtin <$> basicTest
  , Builtin <$> productTest
  , Builtin <$> productWithMultipleTest
  , Builtin <$> nestedProductTest
  ]

expressionTests :: [TestCase Expression]
expressionTests =
  [ applicationTest
  , recursiveTest
  ]

basicTest :: TestCase Schema
basicTest = TestCase
  { _tgType = SInteger
  , _tgValid = [Number (-1), Number 0, Number 1, Number 2]
  , _tgInvalid = [Bool True, String "foo"]
  }

productTest :: TestCase Schema
productTest = TestCase
  { _tgType = ProductType (HM.fromList
    [ ("foo", Builtin SInteger)
    ])
  , _tgValid =
    [ object [ "foo" .= Number 123 ]
    , object [ "foo" .= Number 123, "bar" .= String "baz" ]
    ]
  , _tgInvalid =
    [ Bool True
    , object [ "foo" .= Null ]
    , object [ "foo" .= String "bar" ]
    , Object mempty
    ]
  }

productWithMultipleTest :: TestCase Schema
productWithMultipleTest = TestCase
  { _tgType = ProductType (HM.fromList
    [ ("foo", Builtin SInteger)
    , ("bar", Builtin SInteger)
    ])
  , _tgValid =
    [ object [ "foo" .= Number 123, "bar" .= Number 123 ]
    ]
  , _tgInvalid =
    [ object [ "foo" .= Number 123 ]
    , object [ "foo" .= Number 123, "bar" .= String "baz" ]
    ]
  }

nestedProductTest :: TestCase Schema
nestedProductTest = TestCase
  { _tgType = ProductType (HM.fromList
    [ ("foo", Builtin (ProductType (HM.fromList
                [ ("bar", Builtin SInteger)
                ]))
      )
    ])
  , _tgValid =
    [ object [ "foo" .= object [ "bar" .= Number 123 ] ]
    ]
  , _tgInvalid =
    [ object [ "foo" .= Number 123 ]
    , object [ "foo" .= object [ "bar" .= String "baz" ] ]
    ]
  }

--------------------------------------------------
-- Expression tests
--------------------------------------------------

applicationTest :: TestCase Expression
applicationTest = TestCase
  { _tgType = Application
    (Abstraction "a" (Variable "a"))
    (Builtin SInteger)
  , _tgValid = [Number 123]
  , _tgInvalid = [Bool True, String "abc"]
  }

recursiveTest :: TestCase Expression
recursiveTest = TestCase
  { _tgType = Application
    (Abstraction "list" (Variable "list"))
    (Application
      (Abstraction "a" (Builtin (SumType (HM.fromList
        [ ("nil", Builtin (ProductType mempty))
        , ("cons", Builtin (ProductType (HM.fromList
                     [ ("1", Variable "a")
                     , ("2", Variable "list")
                     ])))
        ]))))
      (Builtin SInteger))
  , _tgValid =
    [ object [ "nil" .= Object mempty ]
    , object [ "cons" .= object [ "1" .= Number 123
                                , "2" .= object [ "nil" .= Object mempty ] ]
                                ]
    ]
  , _tgInvalid = [Bool True]
  }
