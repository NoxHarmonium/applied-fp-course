{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic, renderError,
                                           renderContentType, getMockListTopicJson,
                                           getMockGetTopicJson)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType text =
  responseLBS 
    status 
    [("Content-Type", renderContentType contentType)] 
    text


resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 contentType text =
  mkResponse status200 contentType text

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 contentType text =
  mkResponse status404 contentType text

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 contentType text =
  mkResponse status400 contentType text

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t lbs =
  let
    t' = mkTopic t
    c' = mkCommentText $ lazyByteStringToStrictText lbs
  in
    AddRq <$> t' <*> c'
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict


-- do
--   a <- x
--   b <- y
--   c <- z
--   return (f a b c)

--   f <$> x <*> y <*> z

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest text =
  ViewRq <$> (mkTopic text)

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq


mkErrorResponse
  :: Error
  -> Response
mkErrorResponse ResourceDoesNotExist  = resp404 PlainText (renderError ResourceDoesNotExist)
mkErrorResponse TopicDoesNotExist     = resp404 PlainText (renderError TopicDoesNotExist)
mkErrorResponse TopicNameMissing      = resp400 PlainText (renderError TopicNameMissing)
mkErrorResponse CommentTextMissing    = resp400 PlainText (renderError CommentTextMissing)


-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (pathInfo rq, requestMethod rq) of 
    (["topics"], "GET")         -> return $ mkListRequest
    (["topics", topic], "GET")  -> return $ mkViewRequest topic
    (["topics", topic], "POST") -> return $ mkAddRequest topic ""
    (_, _)                      -> return $ Left ResourceDoesNotExist



-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest ListRq                = Right $ resp200 Json getMockListTopicJson
handleRequest (ViewRq topic)        = (Right $ resp200 Json) <*> getMockGetTopicJson topic
handleRequest (AddRq _ _) = Right $ resp200 PlainText "Success"
  
handleErrors
  :: Either Error Response
  -> Response
handleErrors (Right response) = response
handleErrors (Left e)     = mkErrorResponse e

app
  :: Application
app req respond = 
   (mkRequest req) >>= return . handleErrors . (>>= handleRequest) >>= respond

runApp :: IO ()
runApp = run 3000 app
