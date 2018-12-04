{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Based on the logic as implemented in https://github.com/awslabs/aws-lambda-cpp
module AWS.Lambda.RuntimeAPI (
    -- * Entry points
    defaultMain,
    jsonMain,
    -- ** Mocks
    autoMockMain,
    autoMockJsonMain,
    Mock, GenMock (..),
    makeMockRequest,
    -- * Requests and response
    Request,
    Response (..),
    GenRequest (..),
    RequestId,
    -- * Time remaining
    getTimeRemaining,
    -- * JSON responses
    jsonResponse,
    jsonMediaType,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.Async (waitCatch, withAsync)
import Control.DeepSeq          (NFData (..), force)
import Control.Exception        (SomeException, displayException, evaluate)
import Control.Monad            (forever, void)
import Data.ByteString          (ByteString)
import Data.Int                 (Int64)
import Data.Maybe               (fromMaybe)
import Data.Text                (Text)
import Network.HTTP.Media       (MediaType)
import System.Environment       (lookupEnv)
import System.IO                (hFlush, stdout)

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import qualified Data.Time.Clock.System as Time
import qualified Network.HTTP.Client    as H
import qualified Network.HTTP.Media     as HTTP
import qualified Network.HTTP.Types     as HTTP

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Opaque request id
newtype RequestId = RequestId ByteString
  deriving Show

newtype Deadline = Deadline Int64
  deriving Show

-- | Request with 'ByteString' payload
type Request = GenRequest ByteString

data GenRequest a = Request
    { requestPayload         :: !a           -- ^ The user's payload represented (originally) as a UTF-8 string.
    , requestId              :: !RequestId   -- ^ And identifier unique to the current invocation.
    , requestXRayTraceId     :: !ByteString  -- ^ X-Ray tracigng of the current invocation.
    , requestClientContext   :: !ByteString  -- ^ Information about the client application and device when invoked through the AWS Mobile SDK.
    , requestCognitoIdentity :: !ByteString  -- ^ Information about the Amazon Cognito identity provider when invoked through the AWS Mobile SDK.
    , requestARN             :: !ByteString  -- ^ The ARN requested. This can be different in each invoke that executes the same version.
    , requestDeadline        :: !Deadline    -- ^ Function execution deadline counted in milliseconds since the Unix epoch.
    }
  deriving (Show, Functor, Foldable, Traversable)

data Response
    = SuccessResponse !MediaType !BS.ByteString
    | FailureResponse !Text !Text -- ^ error type, error message
  deriving Show

instance NFData Response where
    rnf (SuccessResponse mt bs)  =
        rnf (HTTP.mainType mt) `seq`
        rnf (HTTP.subType mt) `seq`
        rnf (HTTP.parameters mt) `seq`
        rnf bs
    rnf (FailureResponse ty msg) = rnf msg `seq` rnf ty

-- | Create JSON response.
jsonResponse :: Aeson.ToJSON a => a -> Response
jsonResponse a = SuccessResponse jsonMediaType $ LBS.toStrict $ Aeson.encode a

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Get time remaining (milliseconds).
getTimeRemaining :: GenRequest a -> IO Int64
getTimeRemaining Request { requestDeadline = Deadline deadline } = do
    millis <- getCurrentMillis
    return (max 0 (deadline - millis))

-- | Get current time in milliseconds. Useful with 'Mock'.
getCurrentMillis :: IO Int64
getCurrentMillis = do
    Time.MkSystemTime sec nano <- Time.getSystemTime
    return $ sec * 1000 + fromIntegral (nano `div` 1000000)

-------------------------------------------------------------------------------
-- JSON media type
-------------------------------------------------------------------------------

jsonMediaType :: MediaType
jsonMediaType = "application/json"

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

logError :: ByteString -> IO ()
logError str = do
    BS8.putStrLn $ "[ERROR] " <> str
    hFlush stdout

logException :: ByteString -> SomeException -> IO ()
logException str exc = do
    BS8.putStrLn $ "[ERROR] Exception " <> str <> " -- " <> BS8.pack (displayException exc)
    hFlush stdout

logInfo :: ByteString -> IO ()
logInfo _ = return ()

logHttpRequest :: H.Request -> IO ()
logHttpRequest _ = return ()

logHttpResponse :: HttpRes -> IO ()
logHttpResponse _ = return ()

-------------------------------------------------------------------------------
-- Http
-------------------------------------------------------------------------------

httpLbs :: Ctx -> H.Request -> IO HttpRes
httpLbs ctx req = do
    logHttpRequest req
    eres <- tryDeep $ toHttpRes <$> H.httpLbs req (ctxManager ctx)
    case eres of
        Right res -> do
            logHttpResponse res
            return res
        Left exc -> do
            logException "httpLbs" exc
            return $ HttpRes
                { hrStatus  = HTTP.status500
                , hrHeaders = mempty
                , hrBody    = mempty
                }

data HttpRes = HttpRes
    { hrStatus  :: HTTP.Status
    , hrHeaders :: HTTP.ResponseHeaders
    , hrBody    :: LBS.ByteString
    }

instance NFData HttpRes where
    rnf (HttpRes (HTTP.Status x y) z w) =
        rnf x `seq`
        rnf y `seq`
        rnf z `seq`
        rnf w

toHttpRes :: H.Response LBS.ByteString -> HttpRes
toHttpRes res = HttpRes
    { hrStatus  = H.responseStatus res
    , hrHeaders = H.responseHeaders res
    , hrBody    = H.responseBody res
    }

-------------------------------------------------------------------------------
-- trydeep
-------------------------------------------------------------------------------

-- | Erlang inspired tryDeep
tryDeep :: NFData a => IO a -> IO (Either SomeException a)
tryDeep m = withAsync (m >>= evaluate . force) waitCatch

-------------------------------------------------------------------------------
-- Headers
-------------------------------------------------------------------------------

headerRequestId :: HTTP.HeaderName
headerRequestId = "lambda-runtime-aws-request-id"

headerTraceId :: HTTP.HeaderName
headerTraceId = "lambda-runtime-trace-id";

headerClientContext :: HTTP.HeaderName
headerClientContext = "lambda-runtime-client-context";

headerCognitoIdentity :: HTTP.HeaderName
headerCognitoIdentity = "lambda-runtime-cognito-identity";

headerARN :: HTTP.HeaderName
headerARN = "lambda-runtime-invoked-function-arn";

headerDeadline :: HTTP.HeaderName
headerDeadline = "lambda-runtime-deadline-ms";

-------------------------------------------------------------------------------
-- "runtime"
-------------------------------------------------------------------------------

-- | Context. Called runtime in C++ implementation.
data Ctx = Ctx
    { ctxManager        :: H.Manager
    , ctxNextRequest    :: H.Request
    , ctxSuccessRequest :: H.Request
    , ctxErrorRequest   :: H.Request
    }

-- | Initialise the context, i.e. needed things to communicate with AWS Lambda
initCtx :: IO Ctx
initCtx = do
    endpoint <- do
        e <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
        case e of
            Nothing -> fail "AWS_LAMBDA_RUNTIME_API not defined"
            Just e' -> return e'

    nextReq     <- H.parseRequest $ "http://" ++ endpoint ++ "/2018-06-01/runtime/invocation/next"
    successReq  <- H.parseRequest $ "http://" ++ endpoint ++ "/2018-06-01/runtime/invocation/:request/response"
    errorReq    <- H.parseRequest $ "http://" ++ endpoint ++ "/2018-06-01/runtime/invocation/:request/error"

    mgr <- H.newManager H.defaultManagerSettings
        -- TODO: I'm not sure if it's enought to have timeout only on NEXT requests?
        { H.managerResponseTimeout = H.responseTimeoutNone
        }

    return Ctx
        { ctxManager        = mgr
        , ctxNextRequest    = nextReq
        --    { H.responseTimeout = H.responseTimeoutNone
        --    }
        , ctxSuccessRequest = successReq
            { H.method = "POST"
            }
        , ctxErrorRequest   = errorReq
            { H.method = "POST"
            }
        }

--  | Ask lambda for an invocation.
getNext :: Ctx -> IO (Either HTTP.Status Request)
getNext ctx = do
    res <- httpLbs ctx $ ctxNextRequest ctx

    let st = hrStatus res

    if not $ HTTP.statusIsSuccessful st
    then return (Left st)
    else do
        let headers = Map.fromList $ hrHeaders res
            optionalHeader h = fromMaybe mempty $  Map.lookup h headers

        case Map.lookup headerRequestId headers of
            Nothing    -> return $ Left st
            Just reqId -> return $ Right Request
                { requestPayload         = LBS.toStrict (hrBody res)
                , requestId              = RequestId reqId
                , requestXRayTraceId     = optionalHeader headerTraceId
                , requestClientContext   = optionalHeader headerClientContext
                , requestCognitoIdentity = optionalHeader headerCognitoIdentity
                , requestARN             = optionalHeader headerARN
                , requestDeadline        = parseDeadline $ optionalHeader headerDeadline
                }
  where
    parseDeadline bs = Deadline $ maybe 0 (fromInteger . fst) (BS8.readInteger bs)

-- | Tells lambda that the function has succeeded.
postSuccess :: Ctx -> Request -> Response -> IO ()
postSuccess ctx req (SuccessResponse mt bs) =
    void $ httpLbs ctx httpReq
  where
    httpReq :: H.Request
    httpReq = (ctxSuccessRequest ctx)
        { H.path = replaceRequestId (requestId req) $ H.path (ctxSuccessRequest ctx)
        , H.requestHeaders =
            [ ("Content-Type", HTTP.renderHeader mt)
            ]
        , H.requestBody = H.RequestBodyBS bs
        }
postSuccess ctx req (FailureResponse ty msg) =
    postFailure ctx req ty msg


-- | Tells lambda that the function has failed.
postFailure :: Ctx -> Request -> Text -> Text -> IO ()
postFailure ctx req ty msg =
    void $ httpLbs ctx httpReq
  where
    httpReq :: H.Request
    httpReq = (ctxSuccessRequest ctx)
        { H.path = replaceRequestId (requestId req) $ H.path (ctxSuccessRequest ctx)
        , H.requestHeaders =
            [ ("Content-Type", HTTP.renderHeader jsonMediaType)
            ]
        , H.requestBody = H.RequestBodyLBS $ Aeson.encode $ Aeson.object
            [ "errorMessage" Aeson..= msg
            , "errorType"    Aeson..= ty
            , "stackTrace"   Aeson..= Aeson.Array mempty
            ]
        }

replaceRequestId :: RequestId -> ByteString -> ByteString
replaceRequestId (RequestId rid) path = case BS.breakSubstring ":request" path of
    (pfx, sfx) -> pfx <> rid <> BS.drop 8 sfx

-------------------------------------------------------------------------------
-- Main variants
-------------------------------------------------------------------------------

-- | Default main to implement AWS Lambda using Runtime API
defaultMain :: (Request -> IO Response) -> IO ()
defaultMain handler = do
    ctx <- initCtx

    forever $ do
        next <- getNext ctx
        case next of
            -- on failure we log, and loop
            -- TODO: use status
            Left _st -> logError "HTTP request was not successful. HTTP response code: %d. Retrying.."

            -- on success we have a request!
            Right req -> do
                logInfo "Invoking user handler"
                eres <- tryDeep (handler req)
                case eres of
                    Left exc  -> do
                        logException "handler" exc
                        postFailure ctx req "SomeException" (Text.pack $ displayException exc)
                    Right res -> postSuccess ctx req res

-- | Like 'defaultMain' but work with JSON
jsonMain :: (Aeson.FromJSON a, Aeson.ToJSON b) => (GenRequest a -> IO b) -> IO ()
jsonMain handler = defaultMain $ \req ->
    case traverse Aeson.eitherDecodeStrict req of
        Right req' -> jsonResponse <$> handler req'
        Left err   -> return $ FailureResponse "decode" (Text.pack err)

-------------------------------------------------------------------------------
-- Mock variants
-------------------------------------------------------------------------------

type Mock = GenMock ByteString

-- | How to mock the 'Request', what to do with the 'Response'.
data GenMock a = Mock
    { mockRequest  :: IO (GenRequest a)
    , mockResponse :: Response -> IO ()
    }

-- | Create 'Mock' request
makeMockRequest
    :: a               -- ^ payload value
    -> Int64           -- ^ request duration (in milliseconds)
    -> IO (GenRequest a)
makeMockRequest payload duration = do
    currentMillis <- getCurrentMillis
    return $ Request
        { requestPayload         = payload
        , requestId              = RequestId "mock-deadc0de"
        , requestXRayTraceId     = mempty
        , requestClientContext   = mempty
        , requestCognitoIdentity = mempty
        , requestARN             = mempty
        , requestDeadline        = Deadline $ currentMillis + max 1000 duration
        }

-- | Check the existence of @AWS_LAMBDA_RUNTIME_API@ environment variable.
--
-- If it exists, assume we are running in AWS Lambda environment, i.e. run normally.
-- Otherwise use supplied 'MockOptions'
autoMockMain :: Mock -> (Request -> IO Response) -> IO ()
autoMockMain mock handler = do
    e <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
    case e of
        Just _  -> defaultMain handler
        Nothing -> mockRequest mock >>= handler >>= mockResponse mock

-- | Like 'autoMockMain' but works with JSON
autoMockJsonMain :: (Aeson.FromJSON a, Aeson.ToJSON b) => GenMock a -> (GenRequest a -> IO b) -> IO ()
autoMockJsonMain mock handler = do
    e <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
    case e of
        Just _  -> jsonMain handler
        Nothing -> mockRequest mock >>= handler >>= mockResponse mock . jsonResponse
