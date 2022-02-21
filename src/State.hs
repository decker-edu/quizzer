module State where

import Atomically
import Data.Aeson
import qualified Data.Map as Map
import Network.WebSockets
import Relude
import qualified Text.Show as Text

data PollStatus = Ready | Active deriving (Generic, Show)

instance ToJSON PollStatus

-- | A client connection with its id.
type ClientId = Text

type Client = (Text, Connection)

instance Text.Show Connection where
  show _ = "<conn>" :: String

type ClientMap = Map Text Connection

-- | A quiz session.
data Session = Session
  { master :: Connection,
    status :: PollStatus,
    clients :: ClientMap,
    votes :: Map Text [ClientId],
    clientCss :: Text
  }
  deriving (Show)

-- | Quiz sessions are indexed by a key that is just a random string.
type SessionKey = Text

-- |  The map of all active quiz sessions.
type SessionMap = Map SessionKey Session

-- |  The central state of the server.
data CentralData = CentralData
  { baseUrl :: String,
    sessions :: SessionMap
  }

type AC = Atomic CentralData

-- | The server state in a TVar.
type Central = TVar CentralData

-- | Creates a new session with the specified key and master connection
createSession :: SessionKey -> Connection -> CentralData -> CentralData
createSession key conn (CentralData base sessions) =
  let session = Session conn (Ready) (fromList []) (fromList []) ""
   in CentralData base (Map.insert key session sessions)

startPoll :: IsString a => Session -> Either a Session
startPoll session@(Session _ Ready _ _ _) =
  Right $ session {status = Active, votes = fromList []}
startPoll _ =
  Left "Can't start an already active session."

stopPoll :: IsString a => Session -> Either a Session
stopPoll session@(Session _ Active _ _ _) =
  Right $ session {status = Ready}
stopPoll _ =
  Left "Can't stop an inactive session."

resetPoll :: IsString a => Session -> Either a Session
resetPoll session@(Session _ Ready _ _ _) =
  Right $ session {status = Ready, votes = fromList []}
resetPoll _ =
  Left "Can't reset an active session."

compileVotes :: Map Text [ClientId] -> Map Text Int
compileVotes = Map.map length

