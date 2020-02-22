{-# LANGUAGE OverloadedStrings #-}

module Monadoc
  ( main
  )
where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Fixed as Fixed
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Data.Version as Version
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromField as Sql hiding (Binary)
import qualified Database.PostgreSQL.Simple.FromRow as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Database.PostgreSQL.Simple.ToRow as Sql
import qualified GHC.Clock as Clock
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
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf
import qualified Text.Read as Read
import qualified Web.Cookie as Cookie


main :: IO ()
main = do
  say "starting up"
  config <- getConfig
  withConnection $ \connection -> do
    context <- makeContext config connection
    say $ nameVersionCommit context
    Reader.runReaderT runMigrations context
    Async.race_ (runServer context) (runWorker context)


say :: IO.MonadIO m => Text.Text -> m ()
say message = IO.liftIO $ do
  now <- Time.getCurrentTime
  () <- Stm.atomically $ Stm.takeTMVar sayVar
  Text.putStrLn $ formatTime now <> " " <> message
  IO.hFlush IO.stdout
  Stm.atomically $ Stm.putTMVar sayVar ()


sayVar :: Stm.TMVar ()
sayVar = Unsafe.unsafePerformIO $ Stm.newTMVarIO ()
{-# NOINLINE sayVar #-}


formatTime :: Time.UTCTime -> Text.Text
formatTime =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"


data Config = Config
  { configClientId :: Text.Text
  , configClientSecret :: Text.Text
  , configCommit :: Text.Text
  , configPort :: Warp.Port
  , configUrl :: Text.Text
  } deriving (Eq, Show)


getConfig :: IO Config
getConfig = do
  clientId <- getClientId
  clientSecret <- getClientSecret
  commit <- getCommit
  port <- getPort
  url <- getUrl

  pure Config
    { configClientId = clientId
    , configClientSecret = clientSecret
    , configCommit = commit
    , configPort = port
    , configUrl = url
    }


getClientId :: IO Text.Text
getClientId = getEnv "monadoc_client_id"


getClientSecret :: IO Text.Text
getClientSecret = getEnv "monadoc_client_secret"


getCommit :: IO Text.Text
getCommit = getEnv "monadoc_commit"


getPort :: IO Warp.Port
getPort = do
  let name = "monadoc_port" :: String
  string <- Environment.getEnv name
  case Read.readEither string of
    Left message -> fail $ mconcat [name, ": invalid value (", message, ")"]
    Right port -> pure port


getUrl :: IO Text.Text
getUrl = getEnv "monadoc_url"


getEnv :: Text.Text -> IO Text.Text
getEnv = fmap Text.pack . Environment.getEnv . Text.unpack


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


type App = Reader.ReaderT Context IO


runMigrations :: App ()
runMigrations = do
  createMigrationTableIfNecessary
  mapM_ runMigrationIfNecessary migrations


createMigrationTableIfNecessary :: App ()
createMigrationTableIfNecessary = do
  connection <- Reader.asks contextConnection
  rows <- IO.liftIO $ Sql.query_
    connection
    "select count(*) from pg_tables where tablename = 'migrations'"
  case rows of
    [Sql.Only count] | count == (1 :: Int) -> pure ()
    _ -> createMigrationTable


createMigrationTable :: App ()
createMigrationTable = do
  say "[sql] creating migration table"
  connection <- Reader.asks contextConnection
  IO.liftIO . Monad.void $ Sql.execute_
    connection
    "create table migrations (\
    \time timestamp primary key, \
    \digest bytea not null)"


runMigrationIfNecessary :: Migration -> App ()
runMigrationIfNecessary (time, migration) = do
  connection <- Reader.asks contextConnection
  actualDigest <- IO.liftIO . fmap makeDigest $ Sql.formatQuery
    connection
    migration
    ()
  rows <- IO.liftIO $ Sql.query
    connection
    "select digest from migrations where time = ?"
    [time]
  case rows of
    [] -> runMigration time migration actualDigest
    Sql.Only expectedDigest : _ ->
      checkMigration time expectedDigest actualDigest


runMigration :: Time.UTCTime -> Sql.Query -> Digest -> App ()
runMigration time migration digest = do
  say $ "[sql] running migration " <> formatTime time
  connection <- Reader.asks contextConnection
  IO.liftIO $ do
    Monad.void $ Sql.execute_ connection migration
    Monad.void $ Sql.execute
      connection
      "insert into migrations (time, digest) values (?, ?)"
      (time, digest)


checkMigration :: IO.MonadIO m => Time.UTCTime -> Digest -> Digest -> m ()
checkMigration time expected actual =
  IO.liftIO
    . Monad.when (actual /= expected)
    . Exception.throwIO
    $ MigrationDigestMismatch time expected actual


migrations :: [(Time.UTCTime, Sql.Query)]
migrations =
  [ makeMigration
    (2020, 2, 16, 9, 14, 0)
    "create table blobs (\
    \digest bytea primary key, \
    \size integer not null, \
    \content bytea not null)"
  , makeMigration
    (2020, 2, 20, 9, 9, 0)
    "create table github_users (\
    \login text primary key, \
    \token text not null, \
    \guid uuid not null unique)"
  ]


type Migration = (Time.UTCTime, Sql.Query)


makeMigration
  :: (Integer, Int, Int, Int, Int, Fixed.Pico)
  -> Sql.Query
  -> (Time.UTCTime, Sql.Query)
makeMigration (year, month, day, hour, minute, second) query =
  (makeUtcTime year month day hour minute second, query)


makeUtcTime
  :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Time.UTCTime
makeUtcTime year month day hour minute second = Time.UTCTime
  { Time.utctDay = makeDay year month day
  , Time.utctDayTime = Time.timeOfDayToTime $ makeTimeOfDay hour minute second
  }


makeDay :: Integer -> Int -> Int -> Time.Day
makeDay = Time.fromGregorian


makeTimeOfDay :: Int -> Int -> Fixed.Pico -> Time.TimeOfDay
makeTimeOfDay hour minute second = Time.TimeOfDay
  { Time.todHour = hour
  , Time.todMin = minute
  , Time.todSec = second
  }


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
    let byteString = Sql.fromBinary binary :: ByteString.ByteString
    case Crypto.digestFromByteString byteString of
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


runServer :: Context -> IO ()
runServer context =
  Warp.runSettings (settings context) . middleware $ application context


settings :: Context -> Warp.Settings
settings context =
  Warp.defaultSettings
    & Warp.setBeforeMainLoop (beforeMainLoop $ contextConfig context)
    & Warp.setHost host
    & Warp.setOnExceptionResponse (onExceptionResponse context)
    & Warp.setPort (configPort $ contextConfig context)
    & Warp.setServerName (Text.encodeUtf8 $ nameVersionCommit context)


beforeMainLoop :: Config -> IO ()
beforeMainLoop config = say $ Text.unwords
  [ "[server] listening on"
  , Text.pack $ show host
  , "port"
  , Text.pack . show $ configPort config
  ]


host :: Warp.HostPreference
host = "*"


onExceptionResponse :: Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context _ =
  statusResponse Http.internalServerError500 $ defaultHeaders context


nameVersionCommit :: Context -> Text.Text
nameVersionCommit context =
  Text.concat ["monadoc-", version, "+", configCommit $ contextConfig context]


middleware :: Wai.Middleware
middleware = logger . handleEtag


logger :: Wai.Middleware
logger handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    let
      method =
        Text.decodeUtf8With Text.lenientDecode $ Wai.requestMethod request
      path =
        Text.decodeUtf8With Text.lenientDecode
          $ Wai.rawPathInfo request
          <> Wai.rawQueryString request
      status =
        Text.pack . show . Http.statusCode $ Wai.responseStatus response
      duration = Text.pack . Printf.printf "%.3f" $ after - before
    say $ Text.unwords ["[server]", method, path, status, duration]
    respond response


getUserFromCookie :: Context -> Wai.Request -> IO (Maybe GitHubUser)
getUserFromCookie context request =
  case lookup Http.hCookie $ Wai.requestHeaders request of
    Nothing -> pure Nothing
    Just byteString ->
      case lookup "guid" $ Cookie.parseCookiesText byteString of
        Nothing -> pure Nothing
        Just text -> case Uuid.fromText text of
          Nothing -> pure Nothing
          Just guid -> do
            rows <- Sql.query
              (contextConnection context)
              "select * from github_users where guid = ? limit 1"
              [guid]
            case rows of
              [] -> pure Nothing
              gitHubUser : _ -> pure $ Just gitHubUser


setCookieHeader :: Context -> Maybe GitHubUser -> Http.ResponseHeaders
setCookieHeader context maybeGitHubUser = case maybeGitHubUser of
  Nothing -> []
  Just gitHubUser ->
    [ ( Http.hSetCookie
      , Text.encodeUtf8 $ Text.concat
        [ cookieName
        , "="
        , Uuid.toText $ gitHubUserGuid gitHubUser
        , "; HttpOnly; SameSite=Strict"
        , if isHttps . configUrl $ contextConfig context
          then "; Secure"
          else ""
        ]
      )
    ]


cookieName :: Text.Text
cookieName = "guid"


handleEtag :: Wai.Middleware
handleEtag handle request respond = handle request $ \response ->
  let
    isGet = Wai.requestMethod request == Http.methodGet
    isSuccessful = Http.statusIsSuccessful $ Wai.responseStatus response
    expected = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
    hasEtag = Maybe.isJust expected
    actual = lookup Http.hETag $ Wai.responseHeaders response
  in respond $ if isGet && isSuccessful && hasEtag && actual == expected
    then responseBS
      Http.notModified304
      (Wai.responseHeaders response)
      ByteString.empty
    else response


application :: Context -> Wai.Application
application context request respond = do
  maybeGitHubUser <- getUserFromCookie context request
  let
    headers = replaceHeaders (setCookieHeader context maybeGitHubUser)
      $ defaultHeaders context

  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", []) ->
      indexHandler context maybeGitHubUser headers request respond
    ("GET", ["favicon.ico"]) -> faviconHandler headers request respond
    ("GET", ["health-check"]) -> healthCheckHandler headers request respond
    ("GET", ["robots.txt"]) -> robotsHandler headers request respond
    ("GET", ["static", "tachyons-4-11-2.css"]) ->
      tachyonsHandler headers request respond
    ("GET", ["github-callback"]) ->
      gitHubCallbackHandler context headers request respond
    _ -> notFoundHandler headers request respond


type Handler
  = Http.ResponseHeaders
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived


indexHandler :: Context -> Maybe GitHubUser -> Handler
indexHandler context maybeGitHubUser headers _ respond =
  respond . htmlResponse Http.ok200 headers $ do
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
          [Lucid.rel_ "stylesheet", Lucid.href_ "/static/tachyons-4-11-2.css"]
      Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
        Lucid.header_
            [ Lucid.class_
                "bg-purple flex items-center justify-between pa3 white"
            ]
          $ do
              Lucid.h1_ [Lucid.class_ "ma0 normal"] $ Lucid.a_
                [Lucid.class_ "color-inherit no-underline", Lucid.href_ "/"]
                "Monadoc"
              Lucid.div_ [Lucid.class_ ""] $ case maybeGitHubUser of
                Nothing -> Lucid.a_
                  [ Lucid.class_ "color-inherit no-underline"
                  , Lucid.href_ $ Text.concat
                    [ "http://github.com/login/oauth/authorize?client_id="
                    , configClientId $ contextConfig context
                    , "&redirect_uri="
                    , configUrl $ contextConfig context
                    , "/github-callback"
                    ]
                  ]
                  "Log in with GitHub"
                Just gitHubUser ->
                  Lucid.toHtml $ "@" <> gitHubUserLogin gitHubUser
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


faviconHandler :: Handler
faviconHandler headers _ respond = do
  response <- fileResponse
    Http.ok200
    (replaceHeader (Http.hContentType, iconMime) headers)
    "favicon.ico"
  respond response


healthCheckHandler :: Handler
healthCheckHandler headers _ respond =
  respond $ textResponse Http.ok200 headers ""


robotsHandler :: Handler
robotsHandler headers _ respond =
  respond . textResponse Http.ok200 headers $ Text.unlines
    ["User-Agent: *", "Disallow:"]


tachyonsHandler :: Handler
tachyonsHandler headers _ respond = do
  response <- fileResponse
    Http.ok200
    (replaceHeader (Http.hContentType, cssMime) headers)
    "tachyons-4-11-2.css"
  respond response


gitHubCallbackHandler :: Context -> Handler
gitHubCallbackHandler context headers request respond =
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
                [ jsonPair "client_id" . configClientId $ contextConfig context
                , jsonPair "client_secret" . configClientSecret $ contextConfig
                  context
                , jsonPair "code" $ Text.decodeUtf8With Text.lenientDecode code
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
        either fail (pure . gitHubApiLogin)
          . Aeson.eitherDecode
          $ Client.responseBody res
      randomUuid <- Uuid.nextRandom
      let
        newGitHubUser = GitHubUser
          { gitHubUserGuid = randomUuid
          , gitHubUserLogin = login
          , gitHubUserToken = token
          }
      [Sql.Only guid] <- Sql.query
        (contextConnection context)
        "insert into github_users (login, token, guid) values (?, ?, ?) \
      \on conflict (login) do update set token = excluded.token \
      \returning guid"
        newGitHubUser
      let gitHubUser = newGitHubUser { gitHubUserGuid = guid }
      -- TODO: Redirect to where the user wanted to go.
      respond
        . statusResponse Http.found302
        . replaceHeader (Http.hLocation, "/")
        $ replaceHeaders (setCookieHeader context $ Just gitHubUser) headers
    _ -> respond $ statusResponse Http.badRequest400 headers


notFoundHandler :: Handler
notFoundHandler headers _ respond =
  respond $ statusResponse Http.notFound404 headers


data GitHubUser = GitHubUser
  { gitHubUserGuid :: Uuid.UUID
  , gitHubUserLogin :: Text.Text
  , gitHubUserToken :: Text.Text
  } deriving (Eq, Show)


instance Sql.FromRow GitHubUser where
  fromRow = do
    login <- Sql.field
    token <- Sql.field
    guid <- Sql.field
    pure GitHubUser
      { gitHubUserGuid = guid
      , gitHubUserLogin = login
      , gitHubUserToken = token
      }


instance Sql.ToRow GitHubUser where
  toRow gitHubUser =
    [ Sql.toField $ gitHubUserLogin gitHubUser
    , Sql.toField $ gitHubUserToken gitHubUser
    , Sql.toField $ gitHubUserGuid gitHubUser
    ]


newtype GitHubOAuth = GitHubOAuth
  { gitHubOAuthAccessToken :: Text.Text
  } deriving (Eq, Show)


instance Aeson.FromJSON GitHubOAuth where
  parseJSON = Aeson.withObject "GitHubOAuth" $ \object -> do
    accessToken <- requiredJsonKey object "access_token"
    pure GitHubOAuth { gitHubOAuthAccessToken = accessToken }


newtype GitHubApi = GitHubApi
  { gitHubApiLogin :: Text.Text
  } deriving (Eq, Show)


instance Aeson.FromJSON GitHubApi where
  parseJSON = Aeson.withObject "GitHubApi" $ \object -> do
    login <- requiredJsonKey object "login"
    pure GitHubApi { gitHubApiLogin = login }


requiredJsonKey
  :: Aeson.FromJSON v => Aeson.Object -> Text.Text -> Aeson.Parser v
requiredJsonKey = (Aeson..:)


jsonPair :: (Aeson.ToJSON v, Aeson.KeyValue p) => Text.Text -> v -> p
jsonPair = (Aeson..=)


cssMime :: ByteString.ByteString
cssMime = "text/css"


htmlMime :: ByteString.ByteString
htmlMime = "text/html; charset=utf-8"


iconMime :: ByteString.ByteString
iconMime = "image/x-icon"


jsonMime :: ByteString.ByteString
jsonMime = "application/json"


textMime :: ByteString.ByteString
textMime = "text/plain; charset=utf-8"


performRequest
  :: Context
  -> Client.Request
  -> IO (Client.Response LazyByteString.ByteString)
performRequest context initialRequest = do
  let
    userAgent = (Http.hUserAgent, Text.encodeUtf8 $ nameVersionCommit context)
    oldHeaders = Client.requestHeaders initialRequest
    newHeaders = replaceHeader userAgent oldHeaders
    request = initialRequest { Client.requestHeaders = newHeaders }
    method = Text.decodeUtf8With Text.lenientDecode $ Client.method request
    url = Text.pack $ Uri.uriToString id (Client.getUri request) ""
  say $ Text.unwords ["[client]", method, url]
  (response, seconds) <- withDuration . Client.httpLbs request $ contextManager
    context
  let
    status =
      Text.pack . show . Http.statusCode $ Client.responseStatus response
    duration = Text.pack $ Printf.printf "%.3f" seconds
  say $ Text.unwords ["[client]", method, url, status, duration]
  pure response


fileResponse
  :: Http.Status -> Http.ResponseHeaders -> FilePath -> IO Wai.Response
fileResponse status headers file = do
  path <- Package.getDataFileName file
  contents <- ByteString.readFile path
  pure $ responseBS status headers contents


htmlResponse
  :: Http.Status -> Http.ResponseHeaders -> Lucid.Html a -> Wai.Response
htmlResponse status headers =
  responseBS status (replaceHeader (Http.hContentType, htmlMime) headers)
    . LazyByteString.toStrict
    . Lucid.renderBS


statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers = textResponse status headers $ Text.unwords
  [ Text.pack . show $ Http.statusCode status
  , Text.decodeUtf8With Text.lenientDecode $ Http.statusMessage status
  ]


textResponse
  :: Http.Status -> Http.ResponseHeaders -> Text.Text -> Wai.Response
textResponse status headers =
  responseBS status (replaceHeader (Http.hContentType, textMime) headers)
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
    allHeaders = replaceHeaders
      [ (Http.hContentLength, utf8 $ ByteString.length strict)
      , (Http.hETag, utf8 . show $ Crypto.hashWith Crypto.SHA256 strict)
      ]
      headers
  in Wai.responseLBS status allHeaders $ LazyByteString.fromStrict strict


defaultHeaders :: Context -> Http.ResponseHeaders
defaultHeaders context =
  [ ("Content-Security-Policy", contentSecurityPolicy)
  , ("Feature-Policy", featurePolicy)
  , ("Referrer-Policy", "no-referrer")
  , ("Strict-Transport-Security", strictTransportSecurity context)
  , ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "deny")
  ]


strictTransportSecurity :: Context -> ByteString.ByteString
strictTransportSecurity context =
  if isHttps . configUrl $ contextConfig context
    then "max-age=2592000"
    else "max-age=0"


isHttps :: Text.Text -> Bool
isHttps = Text.isPrefixOf "https:"


contentSecurityPolicy :: ByteString.ByteString
contentSecurityPolicy = Text.encodeUtf8
  $ Text.intercalate "; " ["default-src 'none'", "style-src 'self'"]


featurePolicy :: ByteString.ByteString
featurePolicy = Text.encodeUtf8 . Text.intercalate "; " $ fmap
  (<> " 'none'")
  ["camera", "microphone"]


replaceHeader :: Http.Header -> [Http.Header] -> [Http.Header]
replaceHeader new headers = case headers of
  [] -> [new]
  old : rest ->
    if fst new == fst old then new : rest else old : replaceHeader new rest


replaceHeaders :: [Http.Header] -> [Http.Header] -> [Http.Header]
replaceHeaders new old = foldr replaceHeader old new


runWorker :: Context -> IO ()
runWorker _ = Monad.forever $ Concurrent.threadDelay 1000000


withDuration :: IO a -> IO (a, Double)
withDuration action = do
  before <- Clock.getMonotonicTime
  result <- action
  after <- Clock.getMonotonicTime
  pure (result, after - before)
