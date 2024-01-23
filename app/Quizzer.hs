{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Quizzer (main) where

import Atomically
import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.TH
import Data.Digest.Pure.MD5
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.WebSockets
import Network.WebSockets.Snap
import Relude
import Relude.Extra.Map
import Safe (atMay)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.GetOpt as GetOpt
import System.Directory
import System.FilePath ((</>))
import System.Random
import qualified Text.Show as Text

data Opts = Opts
  { _debug :: !Bool,
    _urlBase :: !String
  }

makeLenses ''Opts

data WinnerSelection = FirstVoter | Random deriving (Generic, Show)

$(deriveJSON defaultOptions ''WinnerSelection)

-- | Commands that the presenter sends to the server.
data MasterCommand
  = Start
      { _choices :: ![Text],
        _solution :: !(Maybe [Text]),
        _winnerselection :: !(Maybe WinnerSelection),
        _votes :: !Int
      }
  | Stop
  | Reset
  | Beat
  | ClientCss {_clientCss :: !Text}
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1, omitNothingFields = True} ''MasterCommand)

-- | Commands that the server sends to the voter.
data ClientCommand
  = Begin {_choices :: ![Text], _votes :: !Int}
  | End {_winner :: !Bool}
  | Idle
  | Css {_css :: !Text}
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ClientCommand)

data ErrorMsg = ErrorMsg
  { error :: Text
  }
  deriving (Generic, Show)

instance ToJSON ErrorMsg

type ClientId = Text

type QuizId = Text

-- | The state of a quiz session.
data QuizState
  = Ready
  | Active
      { _choices :: !(Map Text [QuizId]),
        _solution :: ![Text],
        _selection :: !WinnerSelection,
        -- Clients that have given the right answer, reverese ordered by time
        _winners :: ![ClientId],
        _votesPerVoter :: !Int,
        _partialVotes :: !Int,
        _completeVotes :: !Int
      }
  | Finished
      { _choices :: Map Text [QuizId],
        _votesPerVoter :: Int,
        _partialVotes :: Int,
        _completeVotes :: Int
      }
  deriving (Generic, Show)

data QuizStateMsg
  = MsgReady
  | MsgActive
      { _choices :: !(Map Text Int),
        _votesPerVoter :: !Int,
        _partialVotes :: !Int,
        _completeVotes :: !Int
      }
  | MsgFinished
      { _choices :: Map Text Int,
        _votesPerVoter :: Int,
        _partialVotes :: Int,
        _completeVotes :: Int
      }
  deriving (Generic, Show)

instance ToJSON QuizStateMsg where
  toJSON :: QuizStateMsg -> Value
  toJSON MsgReady =
    object ["state" .= ("Ready" :: Text)]
  toJSON (MsgActive choices possible partial complete) =
    object ["state" .= ("Active" :: Text), "choices" .= choices, "votes" .= possible, "partial" .= partial, "complete" .= complete]
  toJSON (MsgFinished choices possible partial complete) =
    object ["state" .= ("Finished" :: Text), "choices" .= choices, "votes" .= possible, "partial" .= partial, "complete" .= complete]

-- | Is sent to the presenter on any change.
data SessionMsg = SessionMsg
  { participants :: !Int,
    quiz :: !QuizStateMsg
  }
  deriving (Generic, Show)

instance ToJSON SessionMsg

data Status' = Ready' | Active' | Done' deriving (Generic, Show)

instance ToJSON Status'

data SessionMsg' = SessionMsg'
  { _state :: !Status',
    _votes :: ![Text],
    _participants :: !Int
  }
  deriving (Generic, Show)

instance ToJSON SessionMsg'

-- makeLenses ''SessionMsg

-- | Sent to the server by the client.
data ClientVote = ClientVote
  { _vote :: [Text]
  }
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ClientVote)

-- | A client connection with its id.
type Client = (Text, Connection)

instance Text.Show Connection where
  show _ = "<conn>" :: String

type ClientMap = Map Text Connection

-- | A quiz session.
data Session = Session
  { _master :: !(Maybe Connection),
    _presenterSecret :: !Text,
    _quizState :: !QuizState,
    _clients :: !ClientMap,
    _clientCss :: !Text
  }
  deriving (Show)

makeLenses ''Session

-- | Quiz sessions are indexed by a key that is just a random string.
type QuizKey = Text

-- |  The map of all active quiz sessions.
type SessionMap = Map QuizKey Session

-- |  The central state of the server.
data CentralData = CentralData
  { -- _baseUrl :: String,
    _sessions :: SessionMap
  }

makeLenses ''CentralData

-- | The server state in a TVar.
type Central = TVar CentralData

type AC = Atomic CentralData

defaultOpts = Opts False ""

options :: [OptDescr (Opts -> Opts)]
options =
  [ GetOpt.Option
      ['d']
      ["debug"]
      (NoArg (set debug True))
      "Write log to ./log instead of /var/log/quizzer",
    GetOpt.Option
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

mkQuizKey :: IO QuizKey
mkQuizKey = mkRandomId 4

mkRandomId :: Int -> IO Text
mkRandomId n = toText . take n . show . md5 . toLazy . show <$> (randomIO :: IO Int)

mkClientId = mkRandomId 8

data QKey = QKey
  { qKey :: !Text
  }
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''QKey)

data MasterToken = MasterToken
  { mtKey :: !Text,
    mtSecret :: !Text
  }
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''MasterToken)

mkMasterToken = do
  secret <- mkClientId
  key <- mkQuizKey
  return (MasterToken key secret)

-- | Handles the master for a new quiz session.
handleMaster :: Central -> PendingConnection -> IO ()
handleMaster central pending = do
  connection <- acceptRequest pending
  putTextLn "Master connection accepted."
  token <- mkMasterToken
  let sessionKey = mtKey token
  modifyCentral' central (createSession sessionKey (mtSecret token) connection)
  putStrLn $ "Session created: " ++ toString sessionKey
  sendTextData connection (encodePretty token)
  sendStatus central sessionKey
  flip
    finally
    ( do
        -- closeClientConnections central sessionKey
        modifyCentral' central (setMasterConnection sessionKey Nothing)
        putStrLn ("Session not destroyed: " ++ toString sessionKey)
    )
    $ forever (masterLoop connection central sessionKey)

-- | Handles the master for a new quiz session.
handleMasterReconnect :: Central -> MasterToken -> PendingConnection -> IO ()
handleMasterReconnect central token@(MasterToken sessionKey secret) pending = do
  sessionSecret <- accessCentral' central (preview (sessions . ix sessionKey . presenterSecret))
  case sessionSecret of
    Just sessionSecret | secret == sessionSecret -> do
      connection <- acceptRequest pending
      modifyCentral' central (setMasterConnection sessionKey (Just connection))
      putTextLn $ "Master session reconnect accepted: " <> sessionKey
      sendTextData connection (encodePretty token)
      sendStatus central sessionKey
      flip
        finally
        ( do
            -- closeClientConnections central sessionKey
            modifyCentral' central (setMasterConnection sessionKey Nothing)
            putStrLn ("Session not destroyed: " ++ toString sessionKey)
        )
        $ forever (masterLoop connection central sessionKey)
    _else -> do
      putStrLn $ "Session does not exist: " ++ toString sessionKey

selectWinner :: Int -> WinnerSelection -> [ClientId] -> Maybe ClientId
selectWinner _ _ [] = Nothing
selectWinner _ FirstVoter winners = listToMaybe (reverse winners)
selectWinner rnd Random winners = do
  let i = abs rnd `mod` length winners
  atMay winners i

masterLoop :: Connection -> Central -> QuizKey -> IO ()
masterLoop connection central key = do
  bytes <- receiveData connection
  let cmd = eitherDecode bytes
  case cmd of
    Left err -> sendTextData connection (encode (ErrorMsg $ toText err))
    Right cmd -> do
      putStrLn $ "Received: " <> show cmd
      rnd :: Int <- randomIO
      runAtomically central $ do
        case cmd of
          Start choices solution selection votes -> do
            initSession key choices (fromMaybe [] solution) (fromMaybe FirstVoter selection) votes
            sendAllClients key (Begin choices votes)
          Stop -> do
            qs <- preuse (sessions . ix key . quizState)
            case qs of
              Just (Active choices _ selection winners possible partial complete) -> do
                assign (sessions . ix key . quizState) (Finished choices possible partial complete)
                let winner = selectWinner rnd selection winners
                commit $ putStrLn $ "winner selection: " <> show selection
                sendEndClients key winner
              _anything -> return ()
          Reset -> do
            assign (sessions . ix key . quizState) Ready
            sendAllClients key Idle
          ClientCss css -> do
            assign (sessions . ix key . clientCss) css
          Beat -> return ()

        sendMasterStatus key

initSession :: QuizKey -> [Text] -> [Text] -> WinnerSelection -> Int -> AC ()
initSession key choices solution selection possible = do
  assign
    (sessions . ix key . quizState)
    (Active (fromList $ map (,[]) choices) (sort solution) selection [] possible 0 0)

closeClientConnections :: Central -> QuizKey -> IO ()
closeClientConnections central key =
  runAtomically central $ do
    clients <- use (sessions . ix key . clients)
    -- keep sessions alive indefinitley for possible reconnects
    -- assign (sessions . at key) Nothing
    commit $ mapM_ (`sendClose` ("Bye." :: Text)) clients

sendMasterStatus :: QuizKey -> AC ()
sendMasterStatus key = do
  session <- fromJust <$> preuse (sessions . ix key)
  let n = length (session ^. clients)
  let msg = SessionMsg n (countVotes (session ^. quizState))
  case session ^. master of
    Just conn ->
      commit $ sendTextData conn (encodePretty msg)
    Nothing -> return ()

countVotes Ready = MsgReady
countVotes (Active choices _ _ _ possible partial complete) = MsgActive (Map.map length choices) possible partial complete
countVotes (Finished choices possible partial complete) = MsgFinished (Map.map length choices) possible partial complete

sendAllClients :: QuizKey -> ClientCommand -> AC ()
sendAllClients key cmd = do
  clients <- use (sessions . ix key . clients)
  commit $ mapM_ (sendClientCommand cmd) clients

sendEndClients :: QuizKey -> Maybe ClientId -> AC ()
sendEndClients key winner = do
  clients <- use (sessions . ix key . clients)
  commit
    $ mapM_
      ( \(cid, connection) ->
          sendClientCommand (End (Just cid == winner)) connection
      )
      (Map.toList clients)

sendClientCommand :: ClientCommand -> Connection -> IO ()
sendClientCommand cmd conn = sendTextData conn (encodePretty cmd)

-- send current session status to master
sendStatus :: Central -> QuizKey -> IO ()
sendStatus central key = do
  putStrLn $ "sending status to master: " <> show key
  session <- fromJust <$> accessCentral' central (view (sessions . at key))
  let n = length (session ^. clients)
  let msg = SessionMsg n (countVotes (session ^. quizState))
  case session ^. master of
    Just conn -> sendTextData conn (encodePretty msg)
    Nothing -> return ()

-- | Handles a new client for an existing quiz session.
handleQuiz :: Central -> Snap ()
handleQuiz central = do
  key <- decodeUtf8 . fromJust <$> getParam "quiz-key"
  -- cid <- decodeUtf8 . rqClientAddr <$> getRequest
  cid <- liftIO mkClientId
  runWebSocketsSnap $ handleClient central key cid

handleClient :: Central -> QuizKey -> Text -> PendingConnection -> IO ()
handleClient central key cid pending = do
  exists <- accessCentral' central (doesSessionExist key)
  if not exists
    then rejectRequest pending (fromLazy $ encode (ErrorMsg $ "No such session: " <> show key))
    else acceptRequest pending >>= clientMain central key cid

clientMain :: Central -> QuizKey -> Text -> Connection -> IO ()
clientMain central key cid connection = do
  putTextLn "Client connection accepted."
  let client = (cid, connection)
  modifyCentral' central (addClient key client)
  quizState <- accessCentral' central (preview (sessions . ix key . quizState))
  css <- accessCentral' central (preview (sessions . ix key . clientCss))
  case css of
    Just css -> do
      sendClientCommand (Css css) connection
    Nothing -> return ()
  sendStatus central key
  case quizState of
    Nothing -> return ()
    Just quizState -> do
      case quizState of
        Active choices _ _ _ nvotes _ _ -> do
          sendClientCommand (Begin (keys choices) nvotes) connection
        Finished {} -> sendClientCommand (End False) connection
        Ready -> sendClientCommand Idle connection
      flip
        finally
        ( do
            cd <- stateCentral central (removeClient key cid . unregisterAnswer key cid)
            when (isSessionActive key cd) $ sendStatus central key
            putStrLn ("Client removed: " ++ toString key ++ ": " ++ show cid)
        )
        $ forever (clientLoop client central key)

clientLoop :: Client -> Central -> QuizKey -> IO ()
clientLoop (cid, connection) central key = do
  answer <- eitherDecode <$> receiveData connection
  case answer of
    Left err -> do
      putStrLn $ "ERROR: " <> err
      sendTextData connection (encode (ErrorMsg $ toText err))
    Right (ClientVote choices) -> do
      modifyCentral' central (registerAnswer key cid choices . unregisterAnswer key cid)
      sendStatus central key

accessCentral' :: Central -> (CentralData -> a) -> IO a
accessCentral' central func = func <$> readTVarIO central

modifyCentral' :: Central -> (CentralData -> CentralData) -> IO ()
modifyCentral' central func = atomically $ modifyTVar' central func

stateCentral :: Central -> (CentralData -> CentralData) -> IO CentralData
stateCentral central func = atomically (modifyTVar' central func >> readTVar central)

-- | Returns True if the session is active.
isSessionActive :: QuizKey -> CentralData -> Bool
isSessionActive key central =
  case preview (sessions . ix key . quizState) central of
    Just (Active {}) -> True
    _otherwise -> False

-- | Add the client if the specified session exists.
addClient :: QuizKey -> Client -> CentralData -> CentralData
addClient key (cid, conn) =
  set (sessions . at key . _Just . clients . at cid) (Just conn)

setMasterConnection :: QuizKey -> Maybe Connection -> CentralData -> CentralData
setMasterConnection key connection = set (sessions . at key . _Just . master) connection

-- | Remove the client if the specified session exists.
-- TODO Remove the votes this client placed
removeClient :: QuizKey -> Text -> CentralData -> CentralData
removeClient key cid =
  set (sessions . at key . _Just . clients . at cid) Nothing

-- | Does the specified session exist?
doesSessionExist :: QuizKey -> CentralData -> Bool
doesSessionExist key = has (sessions . ix key)

-- | Creates a new session with the specified key and master connection
createSession :: QuizKey -> Text -> Connection -> CentralData -> CentralData
createSession key secret conn central =
  let session = Session (Just conn) secret Ready (fromList []) ""
   in set (sessions . at key) (Just session) central

-- | Removes a session.
-- removeSession :: QuizKey -> CentralData -> CentralData
registerAnswer key cid answers central =
  case preview (sessions . ix key . quizState) central of
    Just (Active choices solution selection winners possible partial complete) ->
      let cleared = Map.map (filter (/= cid)) choices
          updated = foldl' (flip (alter (fmap (cid :)))) cleared answers
          complete' = if length answers == possible then complete + 1 else complete
          partial' = if not (null answers) then partial + 1 else partial
          winners' = if sort answers == solution then cid : winners else winners
       in set (sessions . at key . _Just . quizState) (Active updated solution selection winners' possible partial' complete') central
    _noSuchSession -> central

unregisterAnswer :: QuizKey -> ClientId -> CentralData -> CentralData
unregisterAnswer key cid central =
  case preview (sessions . ix key . quizState) central of
    Just (Active choices solution selection winners possible partial complete) ->
      let votes = length $ filter (== cid) $ concat $ Map.elems choices
          cleared = Map.map (filter (/= cid)) choices
          complete' = if votes == possible then complete - 1 else complete
          partial' = if votes /= 0 then partial - 1 else partial
          winners' = filter (/= cid) winners
       in set (sessions . at key . _Just . quizState) (Active cleared solution selection winners' possible partial' complete') central
    _otherwise -> central

routes :: Central -> Snap ()
routes central =
  route
    [ ( "/quiz/:quiz-key/:presenter-secret",
        method GET $ do
          key <- decodeUtf8 . fromJust <$> getParam "quiz-key"
          secret <- decodeUtf8 . fromJust <$> getParam "presenter-secret"
          let token = MasterToken key secret
          runWebSocketsSnap $ handleMasterReconnect central token
      ),
      ("/quiz/:quiz-key", method GET $ handleQuiz central),
      ("/quiz", method GET $ runWebSocketsSnap $ handleMaster central),
      -- ("/", ifTop $ serveFileAs "text/html" "README.html"),
      ("/presenter.html", serveFileAs "text/html" "static/presenter.html"),
      ("/client", serveFileAs "text/html" "static/client.html"),
      ("/", serveFileAs "text/html" "static/client.html")
    ]

main :: IO ()
main = do
  opts <- quizzerOpts <$> getArgs
  case opts of
    Right opts -> do
      let logBaseDir =
            if view debug opts
              then "./log"
              else "/var/log/quizzer"
      createDirectoryIfMissing True logBaseDir
      let config =
            setPort 3003
              $ setAccessLog (ConfigFileLog (logBaseDir </> "access.log"))
              $ setErrorLog (ConfigFileLog (logBaseDir </> "error.log")) mempty ::
              Config Snap ()
      central <- newTVarIO $ CentralData (fromList [])
      simpleHttpServe config (routes central)
    Left err -> do
      putTextLn $ "Usage: quizzer [OPTION...]" <> err
      exitFailure

{--
accessCentralIO :: Central -> (CentralData -> IO a) -> Snap a
accessCentralIO central func = liftIO $ accessCentralIO' central func

accessCentralIO' :: Central -> (CentralData -> IO a) -> IO a
accessCentralIO' central func = readTVarIO central >>= func

modifyCentral :: Central -> (CentralData -> CentralData) -> Snap ()
modifyCentral central func = liftIO $ modifyCentral' central func

-- | Clear the votes
clearVotes :: QuizKey -> CentralData -> CentralData
clearVotes key =
  set (sessions . at key . _Just . votes) (fromList [])

finishWithAuthError =
  finishWith $ setResponseStatus 401 "Not authorized" emptyResponse

finishWithSessionError =
  finishWith $ setResponseStatus 404 "No session available" emptyResponse

accessCentral :: Central -> (CentralData -> a) -> Snap a
accessCentral central func = liftIO $ accessCentral' central func

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
--}
