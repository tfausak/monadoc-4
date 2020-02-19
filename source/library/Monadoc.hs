{-# LANGUAGE OverloadedStrings #-}

module Monadoc
  ( main
  )
where

import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Fixed as Fixed
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromField as Sql hiding (Binary)
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Lucid
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Environment as Environment
import qualified System.IO as IO


main :: IO ()
main = do
  say "starting up"
  config <- getConfig
  say $ Text.unwords ["monadoc", version, configCommit config]
  withConnection $ \connection -> do
    runMigrations connection
    context <- makeContext config connection
    Warp.runSettings (settings config) . middleware $ application context


say :: Text.Text -> IO ()
say message = do
  now <- Time.getCurrentTime
  Text.putStrLn $ formatTime now <> " " <> message
  IO.hFlush IO.stdout


formatTime :: Time.UTCTime -> Text.Text
formatTime =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"


data Config = Config
  { configClientId :: Text.Text
  , configClientSecret :: Text.Text
  , configCommit :: Text.Text
  } deriving (Eq, Show)


getConfig :: IO Config
getConfig = do
  clientId <- getClientId
  clientSecret <- getClientSecret
  commit <- getCommit
  pure Config
    { configClientId = clientId
    , configClientSecret = clientSecret
    , configCommit = commit
    }


getClientId :: IO Text.Text
getClientId = fmap Text.pack $ Environment.getEnv "monadoc_client_id"


getClientSecret :: IO Text.Text
getClientSecret = fmap Text.pack $ Environment.getEnv "monadoc_client_secret"


getCommit :: IO Text.Text
getCommit = fmap Text.pack $ Environment.getEnv "monadoc_commit"


version :: Text.Text
version = Text.pack $ Version.showVersion Package.version


withConnection :: (Sql.Connection -> IO a) -> IO a
withConnection =
  Exception.bracket (Sql.connectPostgreSQL ByteString.empty) Sql.close


data Context = Context
  { contextConfig :: Config
  , contextConnection :: Sql.Connection
  , contextManager :: Client.Manager
  }


makeContext :: Config -> Sql.Connection -> IO Context
makeContext config connection = do
  manager <- Tls.newTlsManager
  pure Context
    { contextConfig = config
    , contextConnection = connection
    , contextManager = manager
    }


runMigrations :: Sql.Connection -> IO ()
runMigrations connection = do
  rows <- Sql.query_
    connection
    "select count(*) from pg_tables where tablename = 'migrations'"
  case rows of
    [Sql.Only count] | count == (1 :: Int) -> pure ()
    _ -> do
      say "creating migration table"
      Monad.void $ Sql.execute_
        connection
        "create table migrations (\
        \time timestamp primary key, \
        \digest bytea not null)"
  mapM_ (runMigration connection) migrations


runMigration :: Sql.Connection -> Migration -> IO ()
runMigration connection (time, migration) = do
  actualDigest <- fmap makeDigest $ Sql.formatQuery connection migration ()
  rows <- Sql.query
    connection
    "select digest from migrations where time = ?"
    [time]
  case rows of
    [] -> do
      say $ "running migration " <> formatTime time
      Monad.void $ Sql.execute_ connection migration
      Monad.void $ Sql.execute
        connection
        "insert into migrations (time, digest) values (?, ?)"
        (time, actualDigest)
    Sql.Only expectedDigest : _ ->
      Monad.when (actualDigest /= expectedDigest)
        . Exception.throwIO
        $ MigrationDigestMismatch time expectedDigest actualDigest


migrations :: [(Time.UTCTime, Sql.Query)]
migrations =
  [ makeMigration
      (2020, 2, 16, 9, 14, 0)
      "create table blobs (\
      \digest bytea primary key, \
      \size integer not null, \
      \content bytea not null)"
  ]


type Migration = (Time.UTCTime, Sql.Query)


makeMigration
  :: (Integer, Int, Int, Int, Int, Fixed.Pico)
  -> Sql.Query
  -> (Time.UTCTime, Sql.Query)
makeMigration (year, month, day, hour, minute, second) query =
  ( Time.UTCTime
    { Time.utctDay = Time.fromGregorian year month day
    , Time.utctDayTime = Time.timeOfDayToTime Time.TimeOfDay
      { Time.todHour = hour
      , Time.todMin = minute
      , Time.todSec = second
      }
    }
  , query
  )


data MigrationDigestMismatch
  = MigrationDigestMismatch Time.UTCTime Digest Digest
  deriving (Eq, Show)


instance Exception.Exception MigrationDigestMismatch


newtype Digest = Digest
  { unwrapDigest :: Crypto.Digest Crypto.SHA256
  } deriving (Eq, Show)


instance Sql.FromField Digest where
  fromField field maybeByteString = do
    binary <- Sql.fromField field maybeByteString
    let _ = binary :: Sql.Binary ByteString.ByteString
    case Crypto.digestFromByteString $ Sql.fromBinary binary of
      Nothing -> Sql.returnError Sql.ConversionFailed field "invalid digest"
      Just digest -> pure $ Digest digest


instance Sql.ToField Digest where
  toField =
    Sql.toField
      . Sql.Binary
      . (\byteString -> byteString :: ByteString.ByteString)
      . ByteArray.convert
      . unwrapDigest


makeDigest :: ByteString.ByteString -> Digest
makeDigest = Digest . Crypto.hash


settings :: Config -> Warp.Settings
settings config =
  Warp.defaultSettings
    & Warp.setBeforeMainLoop beforeMainLoop
    & Warp.setHost host
    & Warp.setLogger logger
    & Warp.setOnExceptionResponse onExceptionResponse
    & Warp.setPort port
    & Warp.setServerName (makeServerName config)


beforeMainLoop :: IO ()
beforeMainLoop = say $ Text.unwords
  ["listening on", Text.pack $ show host, "port", Text.pack $ show port]


host :: Warp.HostPreference
host = "*"


logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = say $ Text.unwords
  [ Text.decodeUtf8With Text.lenientDecode $ Wai.requestMethod request
  , Text.decodeUtf8With Text.lenientDecode
  $ Wai.rawPathInfo request
  <> Wai.rawQueryString request
  , Text.pack . show $ Http.statusCode status
  ]


onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500


port :: Warp.Port
port = 8080


makeServerName :: Config -> ByteString.ByteString
makeServerName config =
  Text.encodeUtf8 $ Text.concat ["monadoc-", version, "+", configCommit config]


middleware :: Wai.Middleware
middleware = handleEtag


handleEtag :: Wai.Middleware
handleEtag handle request respond = handle request $ \response ->
  let
    expected = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
    actual = lookup Http.hETag $ Wai.responseHeaders response
  in respond $ case (Wai.requestMethod request, expected, actual) of
    ("GET", Just _, Just _) | expected == actual ->
      responseBS Http.notModified304 [] ByteString.empty
    _ -> response


application :: Context -> Wai.Application
application context request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of

    ("GET", []) -> respond . htmlResponse $ do
      Lucid.doctype_
      Lucid.html_ [Lucid.lang_ "en-US"] $ do
        Lucid.head_ $ do
          Lucid.meta_ [Lucid.charset_ "utf-8"]
          Lucid.meta_
            [ Lucid.name_ "viewport"
            , Lucid.content_ "initial-scale = 1, width = device-width"
            ]
          Lucid.meta_
            [ Lucid.name_ "description"
            , Lucid.content_ "\x1f516 Better Haskell documentation."
            ]
          Lucid.title_ "Monadoc"
          Lucid.link_
            [ Lucid.rel_ "stylesheet"
            , Lucid.href_ "/static/tachyons-4-11-2.css"
            ]
        Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
          Lucid.header_ [Lucid.class_ "bg-purple pa3 white"]
            . Lucid.h1_ [Lucid.class_ "ma0 normal"]
            $ Lucid.a_
                [Lucid.class_ "color-inherit no-underline", Lucid.href_ "/"]
                "Monadoc"
          Lucid.main_ [Lucid.class_ "pa3"]
            $ Lucid.p_ "\x1f516 Better Haskell documentation."
          Lucid.footer_ [Lucid.class_ "mid-gray pa3 tc"]
            . Lucid.p_ [Lucid.class_ "ma0"]
            $ do
                "Powered by "
                Lucid.a_
                  [ Lucid.class_ "color-inherit"
                  , Lucid.href_ "https://github.com/tfausak/monadoc"
                  ]
                  "Monadoc"
                " version "
                Lucid.a_
                    [ Lucid.class_ "color-inherit"
                    , Lucid.href_
                    $ "https://github.com/tfausak/monadoc/releases/tag/"
                    <> version
                    ]
                  $ Lucid.toHtml version
                " commit "
                let commit = configCommit $ contextConfig context
                Lucid.a_
                    [ Lucid.class_ "color-inherit"
                    , Lucid.href_
                    $ "https://github.com/tfausak/monadoc/commit/"
                    <> commit
                    ]
                  . Lucid.toHtml
                  $ Text.take 7 commit
                "."

    ("GET", ["favicon.ico"]) -> do
      response <- fileResponse "image/x-icon" "favicon.ico"
      respond response

    ("GET", ["health-check"]) -> respond $ textResponse Http.ok200 ""

    ("GET", ["robots.txt"]) ->
      respond . textResponse Http.ok200 $ Text.unlines
        ["User-Agent: *", "Disallow:"]

    ("GET", ["static", "tachyons-4-11-2.css"]) -> do
      response <- fileResponse "text/css" "tachyons-4-11-2.css"
      respond response

    ("GET", ["github-callback"]) ->
      case lookup "code" $ Wai.queryString request of
        Just (Just code) -> do
          token <- do
            req <- Client.parseUrlThrow
              "https://github.com/login/oauth/access_token"
            res <- performRequest
              context
              req
                { Client.method = Http.methodPost
                , Client.requestBody =
                  Client.RequestBodyLBS . Aeson.encode $ Aeson.object
                    [ jsonPair "client_id" . configClientId $ contextConfig
                      context
                    , jsonPair "client_secret"
                    . configClientSecret
                    $ contextConfig context
                    , jsonPair "code"
                      $ Text.decodeUtf8With Text.lenientDecode code
                    ]
                , Client.requestHeaders =
                  [(Http.hAccept, jsonMime), (Http.hContentType, jsonMime)]
                }
            either fail (pure . gitHubOAuthAccessToken)
              . Aeson.eitherDecode
              $ Client.responseBody res
          login <- do
            req <- Client.parseUrlThrow "https://api.github.com/user"
            res <- performRequest
              context
              req
                { Client.requestHeaders =
                  [(Http.hAuthorization, "Bearer " <> Text.encodeUtf8 token)]
                }
            either fail (pure . gitHubUserLogin)
              . Aeson.eitherDecode
              $ Client.responseBody res
          -- TODO: Store the login and the token in the database. Return a 302
          -- request to whichever page the user was on. Add a Set-Cookie header
          -- to the response.
          undefined login
        _ -> respond $ statusResponse Http.badRequest400

    _ -> respond $ statusResponse Http.notFound404


newtype GitHubOAuth = GitHubOAuth
  { gitHubOAuthAccessToken :: Text.Text
  } deriving (Eq, Show)


instance Aeson.FromJSON GitHubOAuth where
  parseJSON = Aeson.withObject "GitHubOAuth" $ \object -> do
    accessToken <- requiredJsonKey object "access_token"
    pure GitHubOAuth { gitHubOAuthAccessToken = accessToken }


newtype GitHubUser = GitHubUser
  { gitHubUserLogin :: Text.Text
  } deriving (Eq, Show)


instance Aeson.FromJSON GitHubUser where
  parseJSON = Aeson.withObject "GitHubUser" $ \object -> do
    login <- requiredJsonKey object "login"
    pure GitHubUser { gitHubUserLogin = login }


requiredJsonKey
  :: Aeson.FromJSON v => Aeson.Object -> Text.Text -> Aeson.Parser v
requiredJsonKey = (Aeson..:)


jsonPair :: (Aeson.ToJSON v, Aeson.KeyValue p) => Text.Text -> v -> p
jsonPair = (Aeson..=)


jsonMime :: ByteString.ByteString
jsonMime = "application/json"


performRequest
  :: Context
  -> Client.Request
  -> IO (Client.Response LazyByteString.ByteString)
performRequest context request = do
  let
    method = Text.decodeUtf8With Text.lenientDecode $ Client.method request
    url = Text.pack $ Uri.uriToString id (Client.getUri request) ""
  say $ Text.unwords [method, url]
  response <- Client.httpLbs request $ contextManager context
  let
    status =
      Text.pack . show . Http.statusCode $ Client.responseStatus response
  say $ Text.unwords [method, url, status]
  pure response


fileResponse :: ByteString.ByteString -> FilePath -> IO Wai.Response
fileResponse mime file = do
  path <- Package.getDataFileName file
  contents <- ByteString.readFile path
  pure $ responseBS Http.ok200 [(Http.hContentType, mime)] contents



htmlResponse :: Lucid.Html a -> Wai.Response
htmlResponse =
  responseBS Http.ok200 [(Http.hContentType, "text/html; charset=utf-8")]
    . LazyByteString.toStrict
    . Lucid.renderBS



statusResponse :: Http.Status -> Wai.Response
statusResponse status = textResponse status $ Text.unwords
  [ Text.pack . show $ Http.statusCode status
  , Text.decodeUtf8With Text.lenientDecode $ Http.statusMessage status
  ]


textResponse :: Http.Status -> Text.Text -> Wai.Response
textResponse status =
  responseBS status [(Http.hContentType, "text/plain; charset=utf-8")]
    . Text.encodeUtf8


responseBS
  :: Http.Status
  -> Http.ResponseHeaders
  -> ByteString.ByteString
  -> Wai.Response
responseBS status headers strict =
  let
    utf8 :: Show a => a -> ByteString.ByteString
    utf8 = Text.encodeUtf8 . Text.pack . show
    allHeaders =
      (Http.hContentLength, utf8 $ ByteString.length strict)
        : ( "Content-Security-Policy"
          , "base-uri 'none'; \
          \default-src 'self'; \
          \form-action 'self'; \
          \frame-ancestors 'none'; \
          \object-src 'none'"
          )
        : (Http.hETag, utf8 . show $ Crypto.hashWith Crypto.SHA256 strict)
        : ("Referrer-Policy", "no-referrer")
        : ("X-Content-Type-Options", "nosniff")
        : ("X-Frame-Options", "deny")
        : headers
  in Wai.responseLBS status allHeaders $ LazyByteString.fromStrict strict
