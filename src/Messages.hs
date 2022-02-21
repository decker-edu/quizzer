{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Messages where

import Data.Aeson
import Relude
import State

-- | Commands that the presenter sends to the server.
data MasterCommand
  = Start
      { choices :: [Text],
        votes :: Int
      }
  | Stop
  | Reset
  | ClientCss
      { clientCss :: Text
      }
  deriving (Generic, Show)

instance FromJSON MasterCommand

-- | Commands that the server sends to the voter.
data ClientCommand
  = Begin
      { choices :: [Text],
        votes :: Int
      }
  | End
  | Idle
  | Css
      { css :: Text
      }
  | ClientError {msg :: Text}
  deriving (Generic, Show)

instance ToJSON ClientCommand

-- | Sent to the server by the client.
data ClientVote = ClientVote
  { vote :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON ClientVote

data PollStateMessage
  = PollStateMessage
      { state :: PollStatus,
        votes :: Map Text Int,
        participants :: Int
      }
  | MasterError {msg :: Text}
  deriving (Generic, Show)

instance ToJSON PollStateMessage
