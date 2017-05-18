
module Plate.Schema where

import           Plate.Prelude hiding (evaluate, exp)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Test.QuickCheck

import           Plate.Value

data Expression
  = Variable    Text
  | Abstraction Text Expression
  | Application Expression Expression
  | Builtin     Schema
  deriving (Eq, Show)

data Schema
  = SumType     (HashMap Text Expression)
  | ProductType (HashMap Text Expression)
  | SInteger
  | SSet        Expression
  | SDictionary Expression Expression
  | SSequence   Expression
  | SString
  | SPrimative  Expression
  -- ^ Treat a sum/product type as a dictionary instead of as code.
  --
  -- This is only recommended for special cases, such as when
  -- writing a schema for schemas.
  deriving (Eq, Show)

instance ToPlate Expression where
  toPlate a =
    PPrimative $ case a of
      Variable t -> HM.singleton "variable" (PString t)

      Abstraction t exp ->
        HM.singleton
          "abstraction"
          (PPrimative (HM.fromList [ ("parameter", PString t)
                                   , ("body", toPlate exp)
                                   ]))

      Application exp1 exp2 ->
        HM.singleton
          "application"
          (PPrimative (HM.fromList [ ("function", toPlate exp1)
                                   , ("argument", toPlate exp2)
                                   ]))

      Builtin b -> HM.singleton "type" (toPlate b)

instance FromPlate Expression where
  fromPlate plate =
    case plate of
      (PPrimative hm) ->
        case HM.toList hm of
          [("variable", a)]    -> Variable <$> fromPlate a
          [("abstraction", a)] -> parseAbstraction a
          [("application", a)] -> parseApplication a
          [("type", a)]        -> Builtin <$> fromPlate a
          _ -> Left "FromPlate Expression: No value constructor match"
      _ -> Left "FromPlate Expression: Not a PPrimative"
    where
      parseAbstraction (PPrimative hm) =
        Abstraction
          <$> (fromPlate =<< lkup "parameter" hm)
          <*> (fromPlate =<< lkup "body" hm)
      parseAbstraction _ =
        Left "FromPlate Expression: abstraction is not a sequence"

      parseApplication (PPrimative hm) =
        Application
          <$> (fromPlate =<< lkup "function" hm)
          <*> (fromPlate =<< lkup "argument" hm)
      parseApplication _ =
        Left "FromPlate Expression: application is not a sequence"

      lkup :: Text -> HashMap Text Plate -> Either Text Plate
      lkup t hm =
        maybeToRight ("FromPlate Expression: not found: " <> t) (HM.lookup t hm)

instance ToJSON Expression where
  toJSON = toJSON . toPlate

instance FromJSON Expression where
  parseJSON = parseJSONPlate

instance Arbitrary Expression where
  arbitrary = Builtin <$> arbitrary

instance ToPlate Schema where
  toPlate a =
    case a of
      SumType hm            -> f (PPrimative (toPlate <$> hm))
      ProductType hm        -> f (PPrimative (toPlate <$> hm))
      SInteger              -> f (PPrimative mempty)
      SSet exp              -> f (toPlate exp)
      SDictionary exp1 exp2 -> f (PPrimative (HM.fromList
                                               [ ("keys", toPlate exp1)
                                               , ("values", toPlate exp2)
                                               ]))
      SSequence exp         -> f (toPlate exp)
      SString               -> f (PPrimative mempty)
      SPrimative exp        -> f (toPlate exp)
    where
      f :: Plate -> Plate
      f = PPrimative . HM.singleton ("schema." <> textFromSchema a)

instance FromPlate Schema where
  fromPlate plate =
    case plate of
      (PPrimative hm) ->
        case HM.toList hm of
          -- New value constructors share the same namespace as the builtin
          -- value constructors like @dictionary@, @set@, etc, so to
          -- avoid conflicts we preface everything with @schema.@.
          [("schema.sum", a)]        -> parseSumType a
          [("schema.product", a)]    -> parseProductType a
          [("schema.integer", a)]    -> parseSInteger a
          [("schema.set", a)]        -> SSet <$> fromPlate a
          [("schema.dictionary", a)] -> parseSDictionary a
          [("schema.sequence", a)]   -> SSequence <$> fromPlate a
          [("schema.string", a)]     -> parseSString a
          [("schema.primative", a)]  -> SPrimative <$> fromPlate a
          _ -> Left "FromPlate Schema: No value constructor match"
      _ -> Left "FromPlate Schema: Not a PPrimative"
    where
      parseSumType (PPrimative hm) = SumType <$> traverse fromPlate hm
      parseSumType _ = Left "FromPlate Schema: sum type not PPrimative"

      parseProductType (PPrimative hm) = ProductType <$> traverse fromPlate hm
      parseProductType _ = Left "FromPlate Schema: product type not PPrimative"

      parseSDictionary (PPrimative hm) =
        SDictionary
          <$> (fromPlate =<< lkup "keys" hm)
          <*> (fromPlate =<< lkup "values" hm)
      parseSDictionary _ =
        Left "FromPlate Schema: SDictionary description not a sequence"

      lkup :: Text -> HashMap Text Plate -> Either Text Plate
      lkup t hm =
        maybeToRight ("FromPlate Schema: not found: " <> t) (HM.lookup t hm)

      parseSInteger (PPrimative hm)
        | null hm   = pure SInteger
        | otherwise = fail "SInteger contents must be empty"
      parseSInteger _ =
        Left "FromPlate Schema: SInteger contents not PPrimative"

      parseSString (PPrimative hm)
        | null hm   = pure SString
        | otherwise = fail "SString contents must be empty"
      parseSString _ =
        Left "FromPlate Schema: SString contents not PPrimative"

instance ToJSON Schema where
  toJSON = toJSON . toPlate

instance FromJSON Schema where
  parseJSON = parseJSONPlate

instance Arbitrary Schema where
  arbitrary = sized f
    where
      f :: Int -> Gen Schema
      f 0 = pure SInteger
      f n = do
        (Positive m) <- arbitrary
        let n' = n `div` (m + 1)
        oneof
          [ SumType . HM.fromList . fmap (first T.pack)
              <$> resize n' arbitrary
          , ProductType . HM.fromList . fmap (first T.pack)
              <$> resize n' arbitrary
          , pure SInteger
          , SSet
              <$> resize n' arbitrary
          , SDictionary
              <$> resize n' arbitrary <*> resize n' arbitrary
          , SSequence
              <$> resize n' arbitrary
          , pure SString
          , SPrimative
              <$> resize n' arbitrary
          ]

mapSchema :: (Expression -> Expression) -> Schema -> Schema
mapSchema f s =
  case s of
    SumType     hm        -> SumType (f <$> hm)
    ProductType hm        -> SumType (f <$> hm)
    SInteger              -> s
    SSet        exp       -> SSet (f exp)
    SDictionary exp1 exp2 -> SDictionary (f exp1) (f exp2)
    SSequence   exp       -> SSequence (f exp)
    SString               -> s
    SPrimative  exp       -> SPrimative (f exp)

parseJSONPlate :: FromPlate a => Value -> Parser a
parseJSONPlate a = do
  plate <- parseJSON a
  case fromPlate plate of
    Right exp -> pure exp
    Left e    -> fail (T.unpack e)

textFromSchema :: Schema -> Text
textFromSchema p =
  case p of
    SumType _       -> "sum"
    ProductType _   -> "product"
    SInteger        -> "integer"
    SSet _          -> "set"
    SDictionary _ _ -> "dictionary"
    SSequence _     -> "sequence"
    SString         -> "string"
    SPrimative _    -> "primative"
