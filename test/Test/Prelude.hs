module Test.Prelude
  ( module Export
  , encodePretty
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty hiding (encodePretty)
import Data.HashMap.Strict as Export (HashMap)
import Data.Vector as Export (Vector)
import Protolude as Export

import qualified Data.ByteString.Lazy as LBS

encodePretty :: ToJSON a => a -> ByteString
encodePretty = LBS.toStrict . encodePretty' defConfig { confIndent = Spaces 2 }
