module Actions where

import Atomically
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import Data.Maybe
import Messages
import Network.WebSockets
import Relude
import State

sendSessionStatus :: Central -> SessionKey -> IO ()
sendSessionStatus central key = do
  (Session master status clients votes _) <- fromJust <$> getSession central key
  let n = length clients
  let msg = PollStateMessage status (compileVotes votes) n
  sendTextData master (encodePretty msg)

consumeMasterCommand :: MasterCommand -> Session -> AC Session
consumeMasterCommand (Start choices votes) session =
  case startPoll session of
    Right started -> do
      commit $ do
        sendToClients (Begin choices votes) started
        sendToMaster started
      return started
    Left err -> do
      commit $ sendJson (MasterError err) (master session)
      return session
consumeMasterCommand Stop session = do
  case stopPoll session of
    Right stopped -> do
      commit $ do
        sendToClients End stopped
        sendToMaster stopped
      return stopped
    Left err -> do
      commit $ sendJson (MasterError err) (master session)
      return session
consumeMasterCommand Reset session = do
  case resetPoll session of
    Right reset -> do
      commit $ do
        sendToClients Idle reset
        sendToMaster reset
      return reset
    Left err -> do
      commit $ sendJson (MasterError err) (master session)
      return session
consumeMasterCommand (ClientCss css) session = do
  commit $ sendToClients (Css css) session
  return session

handleMasterError :: Session -> Either Text Session -> AC Session
handleMasterError _ (Right value) = return value
handleMasterError fallback (Left err) = do
  commit $ sendJson (MasterError err) (master fallback)
  return fallback

runAtomicSession :: Central -> SessionKey -> (Session -> AC Session) -> IO ()
runAtomicSession central key action =
  runAtomically central $ readSession key >>= action >>= setSession key

sendToClients :: ClientCommand -> Session -> IO ()
sendToClients cmd session = do
  mapM_ (sendJson cmd) $ Map.elems $ clients session

sendJson :: ToJSON a => a -> Connection -> IO ()
sendJson msg conn = sendTextData conn (encodePretty msg)

sendToMaster :: Session -> IO ()
sendToMaster (Session master status clients votes _) = do
  let n = length clients
  let msg = PollStateMessage status (compileVotes votes) n
  sendTextData master (encodePretty msg)

readSession :: SessionKey -> AC Session
readSession key = fromJust . Map.lookup key <$> gets sessions

setSession :: SessionKey -> Session -> AC ()
setSession key session = modify (\cd -> cd {sessions = Map.insert key session $ sessions cd})

accessCentral :: Central -> (CentralData -> a) -> IO a
accessCentral central func = func <$> readTVarIO central

modifyCentral :: Central -> (CentralData -> CentralData) -> IO ()
modifyCentral central func = atomically $ modifyTVar' central func

getSession :: Central -> SessionKey -> IO (Maybe Session)
getSession central key = accessCentral central (Map.lookup key . sessions)
