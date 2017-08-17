{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Monad.Except               (ExceptT (ExceptT),
                                                     runExceptT, withExceptT)
import           Control.Monad.IO.Class             (liftIO)

import           Network.Wai
import           Network.Wai.Handler.Warp           (run)

import           Data.Either                        (Either (..), either)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.IO                       (hPutStrLn)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           System.IO                          (stderr)

import qualified FirstApp.Conf                      as Conf
import qualified FirstApp.DB                        as DB

import           FirstApp.AppM
import           FirstApp.Error                     (Error (..))
import qualified FirstApp.Responses                 as Res
import           FirstApp.Types

-- Our startup is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire startup process as a whole.
data StartUpError
  = ConfErr Conf.ConfigError
  | DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  appE <- prepareAppReqs
  either print runWithDbConn appE
  where
    runWithDbConn env =
      appWithDb env >> DB.closeDb (envDb env)

    appWithDb env =
      run ( Conf.getPort . Conf.port $ envConfig env) (app env)

prepareAppReqs
  :: IO (Either StartUpError Env)
prepareAppReqs = runExceptT $ do
  cfg <- initConf
  db <- initDB cfg
  pure $ Env cfg db
  where
    toStartUpErr e =
      withExceptT e . ExceptT

    initConf = toStartUpErr ConfErr $
      Conf.parseOptions "appconfig.json"

    initDB cfg = toStartUpErr DbInitErr $
      DB.initDb (Conf.dbFilePath cfg) (Conf.tableName cfg)

app
  :: Env
  -> Application
app env rq cb = do
  e <- requestToResponse
  resp <- either handleError pure e
  cb resp
  where
    logToErr = liftIO . hPutStrLn stderr

    requestToResponse = runAppM env $ do
      mkRequest rq >>= handleRequest

    handleError e = do
      _ <- logToErr $ Text.pack (show e)
      pure $ mkErrorResponse e

handleRequest
  :: RqType
  -> AppM Response
handleRequest rqType = do
  case rqType of
    AddRq t c -> Res.resp200 "Success" <$ DB.addCommentToTopic t c
    ViewRq t  -> Res.resp200Json <$> DB.getComments t
    ListRq    -> Res.resp200Json <$> DB.getTopics

mkRequest
  :: Request
  -- We change this to be in our AppM context as well because when we're
  -- constructing our RqType we might want to call on settings or other such
  -- things, maybe.
  -> AppM RqType
mkRequest rq =
    case ( pathInfo rq, requestMethod rq ) of
      -- Commenting on a given topic
      ( [t, "add"], "POST" ) -> do
        r <- liftIO $ mkAddRequest t <$> strictRequestBody rq
        throwL r
      -- View the comments on a given topic
      ( [t, "view"], "GET" ) -> throwL ( mkViewRequest t )
      -- List the current topics
      ( ["list"], "GET" )    -> throwL mkListRequest
      -- Finally we don't care about any other requests so throw your hands in the air
      _                      -> throwL mkUnknownRouteErr

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 $ LBS.toStrict c)

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkUnknownRouteErr
  :: Either Error RqType
mkUnknownRouteErr =
  Left UnknownRoute

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute     = Res.resp404 "Unknown Route"
mkErrorResponse EmptyCommentText = Res.resp400 "Empty Comment"
mkErrorResponse EmptyTopic       = Res.resp400 "Empty Topic"
mkErrorResponse ( DBError _ )    = Res.resp500 "OH NOES"

