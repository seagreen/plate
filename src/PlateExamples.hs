-- | The first Plate library.

module PlateExamples where

import           Plate.Prelude hiding (bool, either, maybe, sequence)

import           Plate
import qualified Data.HashMap.Strict as HM

unit :: Expression
unit = Builtin (ProductType mempty)

string :: Schema
string =
  SumType (HM.singleton "string" (Builtin (SSequence (Builtin SInteger))))

bool :: Schema
bool =
  SumType (HM.fromList
    [ ("true", unit)
    , ("false", unit)
    ])

maybe :: Expression
maybe =
  Abstraction "a"
    $ Builtin (SumType (HM.fromList
        [ ("just", Variable "a")
        , ("nothing", unit)
        ]))

either :: Expression
either =
  Abstraction "a"
    $ Abstraction "b"
      $ Builtin (SumType (HM.fromList
          [ ("left", Variable "a")
          , ("right", Variable "b")
          ]))

sequence :: Expression
sequence =
  Abstraction "a"
    $ Builtin (SumType (HM.fromList
        [ ("nil", unit)
        , ("cons", Variable "b")
        ]))

dictionary :: Expression
dictionary =
  Abstraction "k"
    $ Abstraction "v"
      $ Application (Variable sequenceRef) tup
  where
    tup :: Expression
    tup =
      Application
        (Application (Variable tuple2Ref) (Variable "k"))
        (Variable "v")

tuple2 :: Expression
tuple2 =
  Abstraction "a"
    $ Abstraction "b"
      $ Builtin (ProductType (HM.fromList
          [ ("1", Variable "a")
          , ("2", Variable "b")
          ]))

expression :: Schema
expression =
  SumType (HM.fromList
    [ ("variable", Builtin SString)
    , ("abstraction",
      Builtin (ProductType (HM.fromList
        [ ("parameter", Builtin SString)
        , ("body", Variable expressionRef)
        ])))
    , ("application",
      Builtin (ProductType (HM.fromList
        [ ("function", Variable expressionRef)
        , ("argument", Variable expressionRef)
        ])))
    , ("type", Variable schemaRef)
    ])

schema :: Schema
schema =
  SumType (HM.fromList
    [ ("schema.sum", sumOrProduct)
    , ("schema.product", sumOrProduct)
    , ("schema.integer", Builtin (ProductType mempty))
    , ("schema.set", Variable expressionRef)
    , ("schema.dictionary",
      Builtin (ProductType (HM.fromList
        [ ("keys", Variable expressionRef)
        , ("values", Variable expressionRef)
        ])))
    , ("schema.sequence", Variable expressionRef)
    , ("schema.string", Builtin (ProductType mempty))
    , ("schema.primative", Variable expressionRef)
    ])
  where
    sumOrProduct :: Expression
    sumOrProduct = Builtin (SPrimative (Variable expressionRef))

exampleLibrary :: [(Text, Expression)]
exampleLibrary =
  [ (stringRef    , Builtin string)
  , (boolRef      , Builtin bool)
  , (maybeRef     , maybe)
  , (eitherRef    , either)
  , (sequenceRef  , sequence)
  , (dictionaryRef, dictionary)
  , (tuple2Ref    , tuple2)
  , (expressionRef, Builtin expression)
  , (schemaRef    , Builtin schema)
  ]

stringRef :: Text
stringRef = "string"

boolRef :: Text
boolRef = "bool"

maybeRef :: Text
maybeRef = "maybe"

eitherRef :: Text
eitherRef = "either"

sequenceRef :: Text
sequenceRef = "sequence"

dictionaryRef :: Text
dictionaryRef = "dictionary"

tuple2Ref :: Text
tuple2Ref = "tuple2"

expressionRef :: Text
expressionRef = "expression"

schemaRef :: Text
schemaRef = "schema"
