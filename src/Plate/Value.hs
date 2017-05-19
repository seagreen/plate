
module Plate.Value where

import           Plate.Prelude hiding (evaluate, exp)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Vector as V
import           Test.QuickCheck

class ToPlate a where
  toPlate :: a -> Plate

class FromPlate a where
  fromPlate :: Plate -> Either Text a

data Plate
  = PPrimitive  (HashMap Text Plate)
  | PInteger    Int
  | PSet        (HashSet Plate)
  | PDictionary (HashMap Plate Plate)
  | PSequence   (Vector Plate)
  | PString     Text
  deriving (Eq, Show)

instance ToPlate Plate where
  toPlate = identity

instance FromPlate Plate where
  fromPlate = pure . identity

instance ToJSON Plate where
  toJSON (PPrimitive hm) =
    case HM.toList hm of
      [("bool", PString "true")] -> Bool True
      [("bool", PString "false")] -> Bool False
      [("nothing", PDictionary x)] -> if HM.null x
                                        then Null
                                        else Object (toJSON <$> hm)
      _ -> Object (toJSON <$> hm)

  toJSON (PInteger n) = Number (fromInteger (toInteger n))

  -- NOTE: Nondeterministic.
  toJSON (PSet xs) = object [ "set" .= toJSON xs ]

  -- NOTE: Nondeterministic.
  toJSON (PDictionary hm) = object [ "dictionary" .= HM.toList hm ]

  toJSON (PSequence xs) = object [ "sequence" .= xs ]

  toJSON (PString t) = String t

instance FromJSON Plate where
  parseJSON v =
    case v of
      Null ->
        pure (PDictionary (HM.singleton (PString "nothing")
                          (PDictionary mempty)))
      Bool b ->
        pure (PDictionary (HM.singleton (PString "bool")
                          (if b
                             then PString "true"
                             else PString "false")))
      String t -> pure (PString t)
      Number _ -> PInteger <$> parseJSON v
      Array _ ->
        fail "Arrays outside of a set/dictionary/etc tag aren't allowed"
      Object hm ->
        case HM.toList hm of
          [("set", b)]        -> PSet <$> parseJSON b
          [("dictionary", b)] -> PDictionary <$> dictionaryFromArray b
          [("sequence", b)]   -> PSequence <$> parseJSON b
          _                   -> PPrimitive <$> parseJSON v

-- | PERFORMANCE: This is just used until I get around to making
-- a 'FromJSONKey' instance for plate.
dictionaryFromArray :: Value -> Parser (HashMap Plate Plate)
dictionaryFromArray =
  withArray "Array-encoded dictionary"
    $ fmap HM.fromList . parseJSON . Array

instance Hashable Plate where
  hashWithSalt salt a =
    case a of
      PPrimitive  b -> hashWithSalt salt b
      PInteger    b -> hashWithSalt salt b
      PSet        b -> hashWithSalt salt b
      PDictionary b -> hashWithSalt salt b
      PSequence   b -> hashWithSalt salt (V.toList b)
      PString     b -> hashWithSalt salt b

instance Arbitrary Plate where
  arbitrary = sized arbPlate
    where
      arbPlate :: Int -> Gen Plate
      arbPlate 0 = PInteger <$> arbitrary
      arbPlate n = do
        (Positive m) <- arbitrary
        let n' = n `div` (m + 1)
        oneof
          [ PPrimitive . HM.fromList . fmap (first T.pack)
              <$> resize n' arbitrary
          , PInteger <$> arbitrary
          , PSet . HashSet.fromList <$> resize n' arbitrary
          , PDictionary . HM.fromList <$> resize n' arbitrary
          , PSequence . V.fromList <$> resize n' arbitrary
          , PString . T.pack <$> arbitrary
          ]

--------------------------------------------------
-- General instances
--------------------------------------------------

instance ToPlate Text where
  toPlate = PString

instance FromPlate Text where
  fromPlate (PString t) = Right t
  fromPlate _ = Left "FromPlate Text: not a PString"
