{-# LANGUAGE OverloadedStrings #-}

module Monadoc (main) where

import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package
import qualified System.Environment as Environment
import qualified System.IO as IO


main :: IO ()
main = do
  say "starting up"
  commit <- getCommit
  let serverName = Text.unwords ["monadoc", version, commit]
  say serverName
  Warp.runSettings (settings serverName) . middleware $ application serverName


say :: Text.Text -> IO ()
say message = do
  now <- Time.getCurrentTime
  Text.putStrLn $ formatTime now <> " " <> message
  IO.hFlush IO.stdout


formatTime :: Time.UTCTime -> Text.Text
formatTime =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"


getCommit :: IO Text.Text
getCommit = do
  maybeString <- Environment.lookupEnv "monadoc_commit"
  pure . Text.pack . take 8 $ Maybe.fromMaybe (replicate 40 '0') maybeString


version :: Text.Text
version = Text.pack $ Version.showVersion Package.version


settings :: Text.Text -> Warp.Settings
settings serverName = Warp.defaultSettings
  & Warp.setBeforeMainLoop beforeMainLoop
  & Warp.setHost host
  & Warp.setLogger logger
  & Warp.setOnExceptionResponse onExceptionResponse
  & Warp.setPort port
  & Warp.setServerName (Text.encodeUtf8 serverName)


beforeMainLoop :: IO ()
beforeMainLoop = say $ Text.unwords
  ["listening on", Text.pack $ show host, "port", Text.pack $ show port]


host :: Warp.HostPreference
host = "*"


logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = say $ Text.unwords
  [ Text.decodeUtf8With Text.lenientDecode $ Wai.requestMethod request
  , Text.decodeUtf8With Text.lenientDecode
    $ Wai.rawPathInfo request <> Wai.rawQueryString request
  , Text.pack . show $ Http.statusCode status
  ]


onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = statusResponse Http.internalServerError500


port :: Warp.Port
port = 8080


middleware :: Wai.Middleware
middleware = handleEtag


handleEtag :: Wai.Middleware
handleEtag handle request respond = handle request $ \ response ->
  let
    expected = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
    actual = lookup Http.hETag $ Wai.responseHeaders response
  in respond $ case (Wai.requestMethod request, expected, actual) of
    ("GET", Just _, Just _) | expected == actual ->
      responseBS Http.notModified304 [] ByteString.empty
    _ -> response


application :: Text.Text -> Wai.Application
application serverName request respond =
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
          Lucid.title_ "Monadoc"
          Lucid.link_
            [Lucid.rel_ "stylesheet", Lucid.href_ "/static/tachyons-4-11-2.css"]
        Lucid.body_ [Lucid.class_ "bg-white black sans-serif"] $ do
          Lucid.div_ [Lucid.class_ "bg-purple pa3 white"]
            . Lucid.h1_ [Lucid.class_ "ma0 normal"]
            $ Lucid.a_
              [Lucid.class_ "color-inherit no-underline", Lucid.href_ "/"]
              "Monadoc"
          Lucid.div_ [Lucid.class_ "pa3"]
            $ Lucid.p_ "\x1f3f7 Better Haskell documentation."
          Lucid.div_ [Lucid.class_ "mid-gray pa3 tc"]
            . Lucid.p_ [Lucid.class_ "ma0"]
            . Lucid.a_
              [ Lucid.class_ "color-inherit"
              , Lucid.href_ "https://github.com/tfausak/monadoc"
              ]
            $ Lucid.toHtml serverName

    ("GET", ["favicon.ico"]) -> do
      response <- fileResponse "image/x-icon" "favicon.ico"
      respond response

    ("GET", ["health-check"]) -> respond $ textResponse Http.ok200 ""

    ("GET", ["robots.txt"]) -> respond . textResponse Http.ok200 $ Text.unlines
      [ "User-Agent: *"
      , "Disallow:"
      ]

    ("GET", ["static", "tachyons-4-11-2.css"]) -> do
      response <- fileResponse "text/css" "tachyons-4-11-2.css"
      respond response

    _ -> respond $ statusResponse Http.notFound404


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
