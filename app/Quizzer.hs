module Quizzer where

import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Digest.Pure.MD5
import Data.Maybe (fromJust)
import Network.WebSockets
import Network.WebSockets.Snap
import Relude.Extra.Map
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.GetOpt as GetOpt
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.Random

data Opts = Opts
  { _debug :: Bool
  , _urlBase :: String
  }

makeLenses ''Opts

defaultOpts = Opts False ""

options :: [OptDescr (Opts -> Opts)]
options =
  [ GetOpt.Option
      ['d']
      ["debug"]
      (NoArg (set debug True))
      "Write log to ./log instead of /var/log/quizzer"
  , GetOpt.Option
      ['u']
      ["url"]
      (ReqArg (set urlBase) "URL")
      "Public base URL of this service"
  ]

quizzerOpts :: [String] -> Either Text Opts
quizzerOpts argv =
  case getOpt Permute options argv of
    (optFuncs, _, []) ->
      Right $ foldl' (\opts func -> func opts) defaultOpts optFuncs
    (_, _, errs) -> Left $ toText $ concat errs

-- | The state of a quiz session.
data QuizState
  = Ready
  | Active { _choices :: Map Text Int }
  | Finished { _choices :: Map Text Int }
  deriving (Generic, Show)

instance ToJSON QuizState where
  toJSON :: QuizState -> Value
  toJSON Ready = object ["state" .= ("Ready" :: Text)]
  toJSON (Active choices) =
    object ["state" .= ("Active" :: Text), "choices" .= choices]
  toJSON (Finished choices) =
    object ["state" .= ("Finished" :: Text), "choices" .= choices]

makeLenses ''QuizState

-- | A client connection with its id.
type Client = (Int, Connection)

-- | A quiz session.
data Session = Session
  { _master :: Connection
  , _quizState :: QuizState
  , _clients :: Map Int Connection
  , _votes :: Map Int Connection
  }

makeLenses ''Session

-- | Quiz sessions are indexed by a key that is just a random string.
type QuizKey = Text

-- | The map of all active quiz sessions.
type SessionMap = Map QuizKey Session

-- | The central state of the server.
data CentralData = CentralData
  { _baseUrl :: String
  , _sessions :: SessionMap
  }

makeLenses ''CentralData

-- | The server state in a TVar.
type Central = TVar CentralData

main :: IO ()
main = do
  opts <- quizzerOpts <$> getArgs
  case opts of
    Right opts -> do
      let logBaseDir =
            if view debug opts
              then "./log"
              else "/var/log/grader"
      createDirectoryIfMissing True logBaseDir
      let config =
            setPort 3003 $
            setAccessLog (ConfigFileLog (logBaseDir </> "access.log")) $
            setErrorLog (ConfigFileLog (logBaseDir </> "error.log")) mempty :: Config Snap ()
      central <- newTVarIO $ CentralData "" (fromList [])
      simpleHttpServe config (routes central)
    Left err -> do
      putTextLn $ "Usage: quizzer [OPTION...]" <> err
      exitFailure

routes :: Central -> Snap ()
routes central =
  route
    [ ("/quizzer/:quiz-key", method GET $ handleQuiz central)
    , ("/quizzer", method GET $ handleSession central)
    , ("/", ifTop $ serveFileAs "text/html" "README.html")
    ]

disableCors :: Snap ()
disableCors = do
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ setHeader "Access-Control-Allow-Methods" "*"

writeJSON :: ToJSON a => a -> Snap ()
writeJSON value = do
  modifyResponse $ setContentType "text/json"
  disableCors
  writeLBS $ encodePretty value

writeData bs = do
  disableCors
  writeBS bs

makeQuizKey :: Snap QuizKey
makeQuizKey =
  toText . take 4 . show . md5 . toLazy . show <$> liftIO (randomIO :: IO Int)

-- | Handle creation of a new session.
handleSession :: Central -> Snap ()
handleSession central = do
  key <- makeQuizKey
  runWebSocketsSnap $ handleMaster central key

data QKey = QKey
  { key :: Text
  } deriving (Generic, Show)

instance ToJSON QKey

-- | Handles the master for a new quiz session.
handleMaster :: Central -> QuizKey -> PendingConnection -> IO ()
handleMaster central key pending = do
  connection <- acceptRequest pending
  putTextLn "Master connection accepted."
  modifyCentral' central (createSession key connection)
  sendTextData connection (encodePretty (QKey key))
  sendStatus central key
  flip finally (modifyCentral' central (removeSession key)) $
    forever (masterLoop connection central key)

data MasterCommand
  = Start { choices :: [Text] }
  | Stop
  | Reset
  deriving (Generic, Show)

instance FromJSON MasterCommand

data ClientCommand
  = Begin { choices :: [Text] }
  | End { choices :: [Text] }
  | Idle
  deriving (Generic, Show)

instance ToJSON ClientCommand

data ErrorMsg = ErrorMsg
  { msg :: Text
  } deriving (Generic, Show)

instance ToJSON ErrorMsg

masterLoop :: Connection -> Central -> QuizKey -> IO ()
masterLoop connection central key = do
  cmd <- eitherDecode <$> receiveData connection
  case cmd of
    Left err -> sendTextData connection (encode (ErrorMsg $ toText err))
    Right cmd -> do
      case cmd of
        Start choices -> do
          setSessionState
            central
            key
            (Active $ fromList $ zip choices (repeat 0))
          sendAllClientCommand central key (Begin choices)
        Stop -> do
          quizState <-
            accessCentral' central (preview (sessions . ix key . quizState))
          case quizState of
            Just (Active choices) -> do
              setSessionState central key (Finished choices)
              sendAllClientCommand central key (End $ keys choices)
            _ -> return ()
        Reset -> do
          setSessionState central key Ready
          sendAllClientCommand central key Idle
      sendStatus central key

setSessionState :: Central -> QuizKey -> QuizState -> IO ()
setSessionState central key state =
  modifyCentral' central (set (sessions . ix key . quizState) state)

sendAllClientCommand :: Central -> QuizKey -> ClientCommand -> IO ()
sendAllClientCommand central key cmd = do
  clients <- accessCentral' central (view (sessions . at key . _Just . clients))
  mapM_ (sendClientCommand cmd) clients

sendClientCommand :: ClientCommand -> Connection -> IO ()
sendClientCommand cmd conn = sendTextData conn (encodePretty cmd)

sendStatus :: Central -> QuizKey -> IO ()
sendStatus central key = do
  session <- fromJust <$> accessCentral' central (view (sessions . at key))
  sendTextData (view master session) (encodePretty (view quizState session))

-- | Handles a new client for an existing quiz session.
handleQuiz :: Central -> Snap ()
handleQuiz central = do
  key <- decodeUtf8 . fromJust <$> getParam "quiz-key"
  runWebSocketsSnap $ handleClient central key

handleClient :: Central -> QuizKey -> PendingConnection -> IO ()
handleClient central key pending = do
  exists <- accessCentral' central (doesSessionExist key)
  if not exists
    then rejectRequest pending ("No such session: " <> encodeUtf8 key)
    else acceptRequest pending >>= clientMain central key

clientMain :: Central -> QuizKey -> Connection -> IO ()
clientMain central key connection = do
  putTextLn "Client connection accepted."
  cid <- randomIO -- Use a random number as client id.
  let client = (cid, connection)
  modifyCentral' central (addClient key client)
  quizState <- accessCentral' central (preview (sessions . ix key . quizState))
  case quizState of
    Nothing -> return ()
    Just quizState -> do
      case quizState of
        Active choices -> sendClientCommand (Begin $ keys choices) connection
        Finished choices -> sendClientCommand (End $ keys choices) connection
        Ready -> sendClientCommand Idle connection
      flip finally (modifyCentral' central (removeClient key cid)) $
        forever (clientLoop client central key)

data ClientVote = ClientVote
  { choice :: Text
  } deriving (Generic, Show)

instance FromJSON ClientVote

clientLoop :: Client -> Central -> QuizKey -> IO ()
clientLoop (cid, connection) central key = do
  answer <- eitherDecode <$> receiveData connection
  case answer of
    Left err -> sendTextData connection (encode (ErrorMsg $ toText err))
    Right (ClientVote choice) -> do
      didVote <- accessCentral' central (didClientVote key cid)
      unless didVote $ do
        modifyCentral' central (registerAnswer key (cid, connection) choice)
        sendStatus central key

finishWithAuthError =
  finishWith $ setResponseStatus 401 "Not authorized" emptyResponse

finishWithSessionError =
  finishWith $ setResponseStatus 404 "No session available" emptyResponse

accessCentral :: Central -> (CentralData -> a) -> Snap a
accessCentral central func = liftIO $ accessCentral' central func

accessCentral' :: Central -> (CentralData -> a) -> IO a
accessCentral' central func = func <$> readTVarIO central

accessCentralIO :: Central -> (CentralData -> IO a) -> Snap a
accessCentralIO central func = liftIO $ accessCentralIO' central func

accessCentralIO' :: Central -> (CentralData -> IO a) -> IO a
accessCentralIO' central func = readTVarIO central >>= func

modifyCentral :: Central -> (CentralData -> CentralData) -> Snap ()
modifyCentral central func = liftIO $ modifyCentral' central func

modifyCentral' :: Central -> (CentralData -> CentralData) -> IO ()
modifyCentral' central func = atomically $ modifyTVar' central func

-- | Add the client if the specified session exists.
addClient :: QuizKey -> Client -> CentralData -> CentralData
addClient key (cid, conn) =
  set (sessions . at key . _Just . clients . at cid) (Just conn)

-- | Remove the client if the specified session exists.
removeClient :: QuizKey -> Int -> CentralData -> CentralData
removeClient key cid =
  set (sessions . at key . _Just . clients . at cid) Nothing

-- | Does the specified session exist?
doesSessionExist :: QuizKey -> CentralData -> Bool
doesSessionExist key = has (sessions . ix key)

didClientVote :: QuizKey -> Int -> CentralData -> Bool
didClientVote key cid = has (sessions . at key . _Just . votes . ix cid)

-- | Creates a new session with the specified key and master connection
createSession :: QuizKey -> Connection -> CentralData -> CentralData
createSession key conn central =
  let session = Session conn Ready (fromList []) (fromList [])
   in set (sessions . at key) (Just session) central

-- | Removes a session.
removeSession :: QuizKey -> CentralData -> CentralData
removeSession key = set (sessions . at key) Nothing

registerAnswer :: QuizKey -> Client -> Text -> CentralData -> CentralData
registerAnswer key (cid, connection) answer central =
  case preview (sessions . ix key . quizState) central of
    Just (Active choices) ->
      set
        (sessions . at key . _Just . quizState)
        (Active (alter (fmap (+ 1)) answer choices)) $
      set (sessions . at key . _Just . votes . at cid) (Just connection) central
    _ -> central
