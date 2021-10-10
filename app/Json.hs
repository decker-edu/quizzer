module Json where

import Relude
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)


data Detail = Detail
  { private1 :: String
  , private2 :: Int
  } deriving (Generic, Show)

instance ToJSON Detail

instance FromJSON Detail

data Public = Public
  { name :: String
  , detail :: Maybe Detail
  } deriving (Generic, Show)

instance ToJSON Public

instance FromJSON Public

onem = Public "One" (Just $ Detail "secret" 42)
twom = Public "One" Nothing

main = do
  print $ encodePretty onem
  print $ encodePretty twom
  print (decode $ encode onem :: Maybe Public)
  print (decode $ encode twom :: Maybe Public)
