{-# LANGUAGE OverloadedStrings #-}

module Monadoc
  ( monadoc
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as Gzip
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
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Data.Version as Version
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.FromField as Sql hiding (Binary)
import qualified Database.PostgreSQL.Simple.FromRow as Sql
import qualified Database.PostgreSQL.Simple.LargeObjects as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Database.PostgreSQL.Simple.ToRow as Sql
import qualified Database.PostgreSQL.Simple.Types as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified GHC.Clock as Clock
import qualified Lucid
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Numeric.Natural as Natural
import qualified Paths_monadoc as Package
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf
import qualified Text.Read as Read
import qualified Web.Cookie as Cookie


monadoc :: IO ()
monadoc = do
  say "initializing"
  config <- getConfig
  context <- makeContext config
  say $ nameVersionCommit context
  runApp context runMigrations
  Async.race_ (runApp context runServer) (runApp context runWorker)


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


formatTime :: Time.FormatTime t => t -> Text.Text
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


packageVersion :: Text.Text
packageVersion = Text.pack $ Version.showVersion Package.version


data Context = Context
  { contextConfig :: Config
  , contextManager :: Client.Manager
  , contextPool :: Pool.Pool Sql.Connection
  }


makeContext :: Config -> IO Context
makeContext config = do
  manager <- Tls.newTlsManager
  pool <- Pool.createPool
    (Sql.connectPostgreSQL ByteString.empty)
    Sql.close
    1
    60
    10

  pure Context
    { contextConfig = config
    , contextManager = manager
    , contextPool = pool
    }


type App = Reader.ReaderT Context IO


runApp :: Context -> App a -> IO a
runApp = flip Reader.runReaderT


runMigrations :: App ()
runMigrations = do
  createMigrationTableIfNecessary
  digests <- fmap Map.fromList
    $ sqlQuery "select time, digest from migrations" ()
  mapM_ (runMigrationIfNecessary digests) migrations


createMigrationTableIfNecessary :: App ()
createMigrationTableIfNecessary = do
  rows <- sqlQuery
    "select count(*) from pg_tables where tablename = 'migrations'"
    ()
  case rows of
    [Sql.Only count] | count == (1 :: Int) -> pure ()
    _ -> createMigrationTable


createMigrationTable :: App ()
createMigrationTable = do
  say "[sql] creating migration table"
  sqlExecute
    "create table migrations (\
    \time timestamp primary key, \
    \digest bytea not null)"
    ()


runMigrationIfNecessary :: Map.Map Time.LocalTime Digest -> Migration -> App ()
runMigrationIfNecessary digests (time, migration) = do
  let actualDigest = makeDigest . toUtf8 $ formatQuery migration
  case Map.lookup time digests of
    Nothing -> runMigration time migration actualDigest
    Just expectedDigest -> checkMigration time expectedDigest actualDigest


runMigration :: Time.LocalTime -> Sql.Query -> Digest -> App ()
runMigration time migration digest = do
  say $ "[sql] running migration " <> formatTime time
  sqlExecute migration ()
  sqlExecute
    "insert into migrations (time, digest) values (?, ?)"
    (time, digest)


checkMigration :: IO.MonadIO m => Time.LocalTime -> Digest -> Digest -> m ()
checkMigration time expected actual =
  Monad.when (actual /= expected) . throw $ MigrationDigestMismatch
    time
    expected
    actual


migrations :: [(Time.LocalTime, Sql.Query)]
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
  , makeMigration
    (2020, 2, 22, 9, 2, 0)
    "create table files (\
    \name text primary key, \
    \digest bytea references blobs)"
  , makeMigration
    (2020, 2, 23, 8, 20, 0)
    "create table responses (\
    \url text primary key, \
    \etag bytea not null)"
  , makeMigration
    (2020, 2, 23, 17, 14, 0)
    "create table preferred_versions (\
    \package text primary key, \
    \range text not null)"
  , makeMigration
    (2020, 2, 29, 11, 53, 0)
    "create table large_objects (\
    \oid oid primary key, \
    \digest bytea not null unique, \
    \size integer not null)"
  , makeMigration
    (2020, 2, 29, 12, 7, 0)
    "create table virtual_files (\
    \name text primary key, \
    \oid oid references large_objects)"
  , makeMigration (2020, 2, 29, 16, 27, 0) "drop table files"
  , makeMigration (2020, 2, 29, 16, 28, 0) "drop table blobs"
  , makeMigration (2020, 3, 3, 21, 42, 0) "truncate table virtual_files"
  , makeMigration (2020, 3, 3, 21, 43, 0) "truncate table large_objects"
  ]


type Migration = (Time.LocalTime, Sql.Query)


makeMigration
  :: (Integer, Int, Int, Int, Int, Fixed.Pico)
  -> Sql.Query
  -> (Time.LocalTime, Sql.Query)
makeMigration (year, month, day, hour, minute, second) query =
  (makeLocalTime year month day hour minute second, query)


makeLocalTime
  :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Time.LocalTime
makeLocalTime year month day hour minute second = Time.LocalTime
  { Time.localDay = makeDay year month day
  , Time.localTimeOfDay = makeTimeOfDay hour minute second
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
  = MigrationDigestMismatch Time.LocalTime Digest Digest
  deriving (Eq, Show)


instance Exception.Exception MigrationDigestMismatch


newtype Digest = Digest
  { unwrapDigest :: Crypto.Digest Crypto.SHA256
  } deriving (Eq, Show)


instance Sql.FromField Digest where
  fromField field maybeByteString = do
    binary <- Sql.fromField field maybeByteString
    case Crypto.digestFromByteString . asByteString $ Sql.fromBinary binary of
      Nothing -> Sql.returnError Sql.ConversionFailed field "invalid digest"
      Just digest -> pure $ Digest digest


instance Sql.ToField Digest where
  toField =
    Sql.toField . Sql.Binary . asByteString . ByteArray.convert . unwrapDigest


asByteString :: ByteString.ByteString -> ByteString.ByteString
asByteString = id


makeDigest :: ByteString.ByteString -> Digest
makeDigest = Digest . Crypto.hash


runServer :: App ()
runServer = do
  say "[server] initializing"
  context <- Reader.ask
  IO.liftIO . Warp.runSettings (settings context) . middleware $ application
    context


settings :: Context -> Warp.Settings
settings context =
  Warp.setBeforeMainLoop (beforeMainLoop $ contextConfig context)
    . Warp.setHost host
    . Warp.setOnExceptionResponse (onExceptionResponse context)
    . Warp.setPort (configPort $ contextConfig context)
    . Warp.setServerName (toUtf8 $ nameVersionCommit context)
    $ Warp.defaultSettings


beforeMainLoop :: Config -> IO ()
beforeMainLoop config = say $ Text.unwords
  [ "[server] listening on"
  , showText host
  , "port"
  , showText $ configPort config
  ]


showText :: Show a => a -> Text.Text
showText = Text.pack . show


host :: Warp.HostPreference
host = "*"


onExceptionResponse :: Context -> Exception.SomeException -> Wai.Response
onExceptionResponse context _ =
  statusResponse Http.internalServerError500 $ defaultHeaders context


nameVersionCommit :: Context -> Text.Text
nameVersionCommit context = Text.concat
  ["monadoc-", packageVersion, "+", configCommit $ contextConfig context]


middleware :: Wai.Middleware
middleware = logger . handleEtag


logger :: Wai.Middleware
logger handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    after <- Clock.getMonotonicTime
    let
      method = fromUtf8 $ Wai.requestMethod request
      path = fromUtf8 $ Wai.rawPathInfo request <> Wai.rawQueryString request
      status = showText . Http.statusCode $ Wai.responseStatus response
      duration = formatDuration $ after - before
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
            rows <- runApp context
              $ sqlQuery "select * from github_users where guid = ?" [guid]
            case rows of
              [] -> pure Nothing
              gitHubUser : _ -> pure $ Just gitHubUser


setCookieHeader :: Context -> Maybe GitHubUser -> Http.ResponseHeaders
setCookieHeader context maybeGitHubUser = case maybeGitHubUser of
  Nothing -> []
  Just gitHubUser ->
    [ ( Http.hSetCookie
      , toUtf8 $ Text.concat
        [ cookieName
        , "="
        , Uuid.toText $ gitHubUserGuid gitHubUser
        , "; HttpOnly; Path=/; SameSite=Strict"
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
    ("GET", ["health-check"]) ->
      healthCheckHandler context headers request respond
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
                  <> packageVersion
                  ]
                $ Lucid.toHtml packageVersion
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


healthCheckHandler :: Context -> Handler
healthCheckHandler context headers _ respond = do
  [Sql.Only one] <- runApp context $ sqlQuery "select 1" ()
  Monad.guard $ one == (1 :: Int)
  respond $ statusResponse Http.ok200 headers


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
                , jsonPair "code" $ fromUtf8 code
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
              [(Http.hAuthorization, "Bearer " <> toUtf8 token)]
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
      [Sql.Only guid] <- runApp context $ sqlQuery
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
  :: IO.MonadIO m
  => Context
  -> Client.Request
  -> m (Client.Response LazyByteString.ByteString)
performRequest context initialRequest = do
  let
    userAgent = (Http.hUserAgent, toUtf8 $ nameVersionCommit context)
    oldHeaders = Client.requestHeaders initialRequest
    newHeaders = replaceHeader userAgent oldHeaders
    request = initialRequest { Client.requestHeaders = newHeaders }
  (response, seconds) <-
    IO.liftIO . withDuration . Client.httpLbs request $ contextManager context
  let
    method = fromUtf8 $ Client.method request
    url = requestUrl request
    status = showText . Http.statusCode $ Client.responseStatus response
    duration = formatDuration seconds
  say $ Text.unwords ["[client]", method, url, status, duration]
  pure response


requestUrl :: Client.Request -> Text.Text
requestUrl request = Text.pack $ Uri.uriToString id (Client.getUri request) ""


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
  [showText $ Http.statusCode status, fromUtf8 $ Http.statusMessage status]


textResponse
  :: Http.Status -> Http.ResponseHeaders -> Text.Text -> Wai.Response
textResponse status headers =
  responseBS status (replaceHeader (Http.hContentType, textMime) headers)
    . toUtf8


responseBS
  :: Http.Status
  -> Http.ResponseHeaders
  -> ByteString.ByteString
  -> Wai.Response
responseBS status headers strict =
  let
    utf8 :: Show a => a -> ByteString.ByteString
    utf8 = toUtf8 . showText
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
contentSecurityPolicy = toUtf8 $ Text.intercalate
  "; "
  ["default-src 'none'", "img-src 'self'", "style-src 'self'"]


featurePolicy :: ByteString.ByteString
featurePolicy =
  toUtf8 . Text.intercalate "; " $ fmap (<> " 'none'") ["camera", "microphone"]


replaceHeader :: Http.Header -> [Http.Header] -> [Http.Header]
replaceHeader new headers = case headers of
  [] -> [new]
  old : rest ->
    if fst new == fst old then new : rest else old : replaceHeader new rest


replaceHeaders :: [Http.Header] -> [Http.Header] -> [Http.Header]
replaceHeaders new old = foldr replaceHeader old new


runWorker :: App ()
runWorker = do
  say "[worker] initializing"
  Monad.forever $ do
    say "[worker] starting loop"
    contents <- updateHackageIndex
    processHackageIndex contents
    say "[worker] finished loop"
    sleep 60


processHackageIndex :: LazyByteString.ByteString -> App ()
processHackageIndex contents = do
  rangesVar <- IO.liftIO $ Stm.newTVarIO Map.empty
  revisionsVar <- IO.liftIO $ Stm.newTVarIO Map.empty
  mapM_ (processTarElement rangesVar revisionsVar)
    . Tar.foldEntries ((:) . Right) [] (pure . Left)
    . Tar.read
    $ Gzip.decompress contents
  ranges <- IO.liftIO $ Stm.readTVarIO rangesVar
  Monad.void
    . sqlHelper
        (\con qry -> fmap (const []) . Sql.executeMany con qry)
        "insert into preferred_versions (package, range) values (?, ?) \
        \on conflict (package) do update set range = excluded.range"
    . fmap (\(pkg, rng) -> (Cabal.unPackageName pkg, Cabal.prettyShow rng))
    $ Map.toList ranges


processTarElement
  :: Stm.TVar (Map.Map Cabal.PackageName Cabal.VersionRange)
  -> Stm.TVar (Map.Map Cabal.PackageIdentifier Natural.Natural)
  -> Either Tar.FormatError Tar.Entry
  -> App ()
processTarElement ranges revisions element = case element of
  Left formatError -> throw formatError
  Right entry -> case Tar.entryContent entry of
    Tar.NormalFile lazyContent _ ->
      let content = LazyByteString.toStrict lazyContent
      in
        case FilePath.splitDirectories $ Tar.entryPath entry of
          [package, "preferred-versions"] -> do
            range <-
              case Cabal.simpleParsec . Text.unpack $ fromUtf8 content of
                Nothing -> if ByteString.null content
                  then pure Cabal.anyVersion
                  else fail $ "invalid preferred versions: " <> show entry
                Just (Cabal.PackageVersionConstraint _ range) -> pure range
            IO.liftIO . Stm.atomically . Stm.modifyTVar ranges $ Map.insert
              (Cabal.mkPackageName package)
              range
          [package, versionString, path] ->
            case FilePath.splitExtensions path of
              (file, ".cabal") | file == package -> do
                let
                  _owner = Tar.ownerName $ Tar.entryOwnership entry
                  _time =
                    Time.posixSecondsToUTCTime . fromIntegral $ Tar.entryTime
                      entry
                  packageName = Cabal.mkPackageName package
                version <- case Cabal.simpleParsec versionString of
                  Nothing -> fail $ "invalid package version: " <> show entry
                  Just version -> pure (version :: Cabal.Version)
                let packageId = Cabal.PackageIdentifier packageName version
                revision <-
                  IO.liftIO
                  . fmap (Map.findWithDefault 0 packageId)
                  $ Stm.readTVarIO revisions
                IO.liftIO
                  . Stm.atomically
                  . Stm.modifyTVar revisions
                  $ Map.insertWith (+) packageId 1
                let
                  packageNameText =
                    Text.pack $ Cabal.unPackageName packageName
                oid <- upsertLargeObject content
                upsertVirtualFile
                  (mconcat
                    [ packageNameText
                    , "/"
                    , Text.pack $ Cabal.prettyShow version
                    , "/"
                    , showText revision
                    , "/"
                    , packageNameText
                    , ".cabal"
                    ]
                  )
                  oid
                pure () -- TODO
              (_, ".json") -> pure ()
              _ -> fail $ "unexpected tar extension: " <> show entry
          _ -> fail $ "unexpected tar path: " <> show entry
    _ -> fail $ "unexpected tar content: " <> show entry


fromUtf8 :: ByteString.ByteString -> Text.Text
fromUtf8 = Text.decodeUtf8With Text.lenientDecode


toUtf8 :: Text.Text -> ByteString.ByteString
toUtf8 = Text.encodeUtf8


throw :: (Exception.Exception e, IO.MonadIO m) => e -> m a
throw = IO.liftIO . Exception.throwIO


sleep :: IO.MonadIO m => Double -> m ()
sleep = IO.liftIO . Concurrent.threadDelay . round . (1000000 *)


updateHackageIndex :: App LazyByteString.ByteString
updateHackageIndex = do
  request <- Client.parseRequest hackageIndexUrl
  response <- performRequestWithEtag request
  case Http.statusCode $ Client.responseStatus response of
    200 -> do
      let content = Client.responseBody response
      oid <- upsertLargeObject $ LazyByteString.toStrict content
      upsertVirtualFile hackageIndexFileName oid
      pure content
    304 -> do
      maybeOid <- selectVirtualFile hackageIndexFileName
      case maybeOid of
        Nothing -> do
          sqlExecute "delete from responses where url = ?" [hackageIndexUrl]
          fail $ "missing index file: " <> show response
        Just oid -> do
          maybeContent <- selectLargeObject oid
          case maybeContent of
            Nothing -> do
              sqlExecute
                "delete from responses where url = ?"
                [hackageIndexUrl]
              deleteVirtualFile hackageIndexFileName
              fail $ "missing index blob: " <> show response
            Just content -> pure $ LazyByteString.fromStrict content
    _ -> fail $ "failed to get Hackage index: " <> show response


performRequestWithEtag
  :: Client.Request -> App (Client.Response LazyByteString.ByteString)
performRequestWithEtag request = do
  let url = requestUrl request
  rows <- sqlQuery "select etag from responses where url = ?" [url]
  let
    oldEtag = maybe ByteString.empty (Sql.fromBinary . Sql.fromOnly)
      $ Maybe.listToMaybe rows
  context <- Reader.ask
  response <- performRequest
    context
    request
      { Client.requestHeaders = replaceHeader (Http.hIfNoneMatch, oldEtag)
        $ Client.requestHeaders request
      }
  case lookup Http.hETag $ Client.responseHeaders response of
    Nothing -> pure ()
    Just newEtag -> sqlExecute
      "insert into responses (url, etag) values (?, ?) \
      \on conflict (url) do update set etag = excluded.etag"
      (url, Sql.Binary newEtag)
  pure response


hackageIndexUrl :: String
hackageIndexUrl = "https://hackage.haskell.org/01-index.tar.gz"


hackageIndexFileName :: Text.Text
hackageIndexFileName = "01-index.tar.gz"


withDuration :: IO a -> IO (a, Double)
withDuration action = do
  before <- Clock.getMonotonicTime
  result <- action
  after <- Clock.getMonotonicTime
  pure (result, after - before)


sqlQuery :: (Sql.ToRow q, Sql.FromRow r) => Sql.Query -> q -> App [r]
sqlQuery = sqlHelper Sql.query


sqlExecute :: Sql.ToRow q => Sql.Query -> q -> App ()
sqlExecute query =
  Monad.void . sqlHelper (\c q -> fmap (const []) . Sql.execute c q) query


sqlHelper
  :: (Sql.Connection -> Sql.Query -> q -> IO [r]) -> Sql.Query -> q -> App [r]
sqlHelper runQuery query substitutions = do
  pool <- Reader.asks contextPool
  (result, duration) <-
    IO.liftIO . withDuration . Pool.withResource pool $ \connection ->
      runQuery connection query substitutions
  say $ Text.unwords
    ["[sql]", formatQuery query, "--", formatDuration duration]
  pure result


formatDuration :: Double -> Text.Text
formatDuration = Text.pack . Printf.printf "%.3f"


formatQuery :: Sql.Query -> Text.Text
formatQuery = fromUtf8 . Sql.fromQuery


upsertLargeObject :: ByteString.ByteString -> App Sql.Oid
upsertLargeObject content = do
  context <- Reader.ask
  IO.liftIO . Pool.withResource (contextPool context) $ \connection ->
    Sql.withTransaction connection . runApp context $ do
      let digest = makeDigest content
      rows <- sqlQuery
        "select oid from large_objects where digest = ?"
        [digest]
      case rows of
        row : _ -> pure $ Sql.fromOnly row
        [] -> do
          oid <- IO.liftIO $ do
            oid <- Sql.loCreat connection
            handle <- Sql.loOpen connection oid Sql.WriteMode
            Monad.void $ Sql.loWrite connection handle content
            Sql.loClose connection handle
            pure oid
          sqlExecute
            "insert into large_objects (oid, digest, size) values (?, ?, ?)"
            (oid, digest, ByteString.length content)
          pure oid


deleteLargeObject :: Sql.Oid -> App ()
deleteLargeObject oid = do
  context <- Reader.ask
  IO.liftIO . Pool.withResource (contextPool context) $ \connection ->
    Sql.withTransaction connection . runApp context $ do
      IO.liftIO $ Sql.loUnlink connection oid
      sqlExecute "delete from large_objects where oid = ?" [oid]


selectLargeObject :: Sql.Oid -> App (Maybe ByteString.ByteString)
selectLargeObject oid = do
  context <- Reader.ask
  IO.liftIO . Pool.withResource (contextPool context) $ \connection ->
    Sql.withTransaction connection . runApp context $ do
      rows <- sqlQuery "select size from large_objects where oid = ?" [oid]
      case rows of
        [] -> pure Nothing
        row : _ -> IO.liftIO $ do
          handle <- Sql.loOpen connection oid Sql.ReadMode
          content <- Sql.loRead connection handle $ Sql.fromOnly row
          Sql.loClose connection handle
          pure $ Just content


upsertVirtualFile :: Text.Text -> Sql.Oid -> App ()
upsertVirtualFile name newOid = do
  context <- Reader.ask
  IO.liftIO . Pool.withResource (contextPool context) $ \connection ->
    Sql.withTransaction connection . runApp context $ do
      maybeOldOid <- selectVirtualFile name
      case maybeOldOid of
        Nothing -> pure ()
        Just oldOid -> do
          rows <- sqlQuery
            "select count(*) from virtual_files where oid = ?"
            [oldOid]
          deleteVirtualFile name
          case rows of
            [Sql.Only count] | count == (1 :: Int) -> deleteLargeObject oldOid
            _ -> pure ()
      sqlExecute
        "insert into virtual_files (name, oid) values (?, ?)"
        (name, newOid)


selectVirtualFile :: Text.Text -> App (Maybe Sql.Oid)
selectVirtualFile name = do
  rows <- sqlQuery "select oid from virtual_files where name = ?" [name]
  pure $ case rows of
    [] -> Nothing
    row : _ -> Just $ Sql.fromOnly row


deleteVirtualFile :: Text.Text -> App ()
deleteVirtualFile name =
  sqlExecute "delete from virtual_files where name = ?" [name]
