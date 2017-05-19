# Definition

Plate is a few things:

### 1. Plate Values

(Keep [Plate.Value](../src/Plate/Value.hs) in sync with this.)

We'd like to use Plate to validate data serialized in different ways, so we avoid tying it into a specific serialization format like JSON.

Instead we build the specification on our own concept of Plate values.

How to define them? If we were really hardcore we'd just say `Value = Map String Value`.

Then for instance we could represent the number 2 as:

```
Map.singleton
  "succ"
  (Map.singleton
    "succ"
    (Map.singleton
      "zero"
      mempty))
```

Of course, we wouldn't want to serialize them this way, the payloads would be huge! Come to think of it we wouldn't want to represent them in memory this way either.

OK, so why not just define them this way for simplicity, then let in-memory and serialization methods use custom representations that are more efficient?

This could work, but makes for an awkward mappings between implementations and the spec. Each implementation would have to coordinate how (eg) integers are represented in the spec, because if one uses "succ" and "zero" and another "next" and "end" then they would inadvertently refer to different values.

This could be solved by adding an adendum to the spec saying "integers are defined as 'succ' and 'zero'". If we're going that far why not just expand the definition of values to include commonly used data types? Then we don't have to make up silly names like "succ" that will always be optimized away anyway.

And so we do.

Plate values are made up of:

1. Primitives (the `Map String Value` mentioned earlier).

2. Integers

3. Sets of Plate values

4. Dictionaries mapping Plate values to Plate values

5. Sequences of Plate values

6. Strings of Unicode code points

### 2. Plate Schemas

(Keep [Plate.Schema](../src/Plate/Value.hs) and [PlateExamples](../src/PlateExamples.hs) in sync with this.)

This is what we use to describe Plate values. A Haskell implementation is shown, though it can be implemented in any language:

```haskell
data Expression
  = Variable    Text
  | Abstraction Text Expression
  | Application Expression Expression
  | Builtin     Schema

data Schema
  = SumType     (HashMap Text Expression)
  | ProductType (HashMap Text Expression)
  | SInteger
  | SSet        Expression
  | SDictionary Expression Expression
  | SSequence   Expression
  | SString
  | SPrimitive  Expression
  -- ^ Treat a sum/product type as a dictionary instead of as code.
  --
  -- This is only recommended for special cases, such as when
  -- writing a schema for schemas.
```

We can also get recursive and describe Plate schemas using Plate schemas:
```haskell
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
    , ("schema.primitive", Variable expressionRef)
    ])
  where
    sumOrProduct :: Expression
    sumOrProduct = Builtin (SPrimitive (Variable expressionRef))
```

### 3. Validation

(Keep [Plate](../src/Plate.hs) in sync with this.)

Take a Plate schema and a Plate value and see if the schema allows the value.

Currently defined by the `validate` function in [Plate](../src/Plate.hs) as well as common sense because there are probably mistakes in `validate`. This definition will improve in the future.

### 4. Mappings from Plate values to serialization formats

(Keep [Plate.Value](../src/Plate/Value.hs) in sync with this.)

This is the highest layer of the specification for a reason -- there will be multiple ways of mapping Plate values to data formats, and that's OK. You just have to keep track of which one was used for each document.

That said, we hope to prevent fragmentation as much as possible by provided well-thought out mappings to most common data formats.

At the moment though only one is provided: a mapping between Plate values and JSON. You can see how its implemented in the `ToJSON` and `FromJSON` instances of [Plate.Value](../src/Plate/Value.hs).

Notably, it maps Plate primitives (`Map String Value`s) to JSON objects, and Plate dictionaries to JSON arrays. This because Plate dictionaries don't work as JSON objects since the latter requires keys to be JSON strings.

This idea came from [Microsoft Bond](https://microsoft.github.io/bond/) which is a fantastic schema language itself (though it doesn't have sum types).

So for example the following Plate value:
```haskell
PPrimitive (HM.singleton
  "left" 
  (PDictionary (HM.fromList
    [ (PInteger 12, PString "Invalid for reason foo")
    , (PInteger 20, PString "Invalid for reason bar")
    ]))
```

Serializes to the JSON:
```json
{ "left":
    { "dictionary":
        [ [ 12
          , "Invalid for reason foo"
          ]
        , [ 20
          , "Invalid for reason bar"
          ]
        ]
    }
}
```

The reason "dictionary" is specified is to distinguish the result from a sequence or a set. This is required to let us roundtrip between the in-memory and serialized versions of the data without ambiguity.

It has two downsides though in that it expands the size of the payload as well as making "dictionary", "set", etc. reserved keywords. The first problem is acceptable, the latter needs more consideration.
