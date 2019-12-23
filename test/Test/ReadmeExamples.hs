module Test.ReadmeExamples where

import Plate
import Test.Prelude

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

write :: IO ()
write = do
  BS.writeFile
    "test/generated/readme-schema.json"
    (encodePretty album)
  BS.writeFile
    "test/generated/readme-instance.json"
    (encodePretty interstellarSoundtrack)
  case validate mempty (Builtin album) interstellarSoundtrack of
    Left e  -> panic (show e)
    Right _ -> pure ()

data Album = Album
  { title  :: Text
  , artist :: Text
  , tracks :: [Text]
  }

album :: Schema
album = ProductType (HM.fromList
  [ ("title", Builtin SString)
  , ("artist", Builtin SString)
  , ("tracks", Builtin (SSequence (Builtin SString)))
  ])

interstellarSoundtrack :: Plate
interstellarSoundtrack = PPrimitive (HM.fromList
  [ ("title", PString "Interstellar: Original Motion Picture Soundtrack")
  , ("artist", PString "Hans Zimmer")
  , ("tracks", PSequence (V.fromList
      [ PString "Dreaming of the Crash"
      , PString "Cornfield Chase"
      , PString "Dust"
      , PString "Day One"
      , PString "Stay"
      , PString "Message from Home"
      , PString "The Wormhole"
      , PString "Mountains"
      , PString "Afraid of Time"
      , PString "A Place Among the Stars"
      , PString "Running Out"
      , PString "I'm Going Home"
      , PString "Coward"
      , PString "Detach"
      , PString "S.T.A.Y."
      , PString "Where We're Going"
      ]))
  ])
