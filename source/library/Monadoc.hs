{-# LANGUAGE OverloadedStrings #-}

module Monadoc (main) where

import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
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
import qualified Network.Wai.Internal as Wai
import qualified Network.Wai.Middleware.Autohead as Middleware
import qualified Paths_monadoc as Package
import qualified System.IO as IO


main :: IO ()
main = do
  say "starting up"
  Warp.runSettings settings $ middleware application


say :: Text.Text -> IO ()
say message = do
  now <- Time.getCurrentTime
  Text.putStrLn $ formatTime now <> " " <> message
  IO.hFlush IO.stdout


formatTime :: Time.UTCTime -> Text.Text
formatTime =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"


settings :: Warp.Settings
settings = Warp.defaultSettings
  & Warp.setBeforeMainLoop beforeMainLoop
  & Warp.setHost host
  & Warp.setLogger logger
  & Warp.setOnExceptionResponse onExceptionResponse
  & Warp.setPort port
  & Warp.setServerName serverName


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


serverName :: ByteString.ByteString
serverName =
  let
    version = Text.encodeUtf8 . Text.pack $ Version.showVersion Package.version
  in "monadoc-" <> version


middleware :: Wai.Middleware
middleware
  = Middleware.autohead
  . addContentLength
  . addSecurityHeaders
  . handleEtag


addContentLength :: Wai.Middleware
addContentLength = Wai.modifyResponse $ \ response -> case response of
  Wai.ResponseBuilder _ _ builder ->
    let
      contentLength = Text.encodeUtf8
        . Text.pack
        . show
        . LazyByteString.length
        $ Builder.toLazyByteString builder
    in Wai.mapResponseHeaders ((Http.hContentLength, contentLength) :) response
  _ -> response


addSecurityHeaders :: Wai.Middleware
addSecurityHeaders = Wai.modifyResponse . Wai.mapResponseHeaders $ mappend
  [ ("Content-Security-Policy", "default-src 'self'")
  , ("Referrer-Policy", "no-referrer")
  , ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "deny")
  ]


handleEtag :: Wai.Middleware
handleEtag handle request respond =
  let expected = lookup Http.hIfNoneMatch $ Wai.requestHeaders request
  in handle request $ \ response ->
    let
      actual = case response of
        Wai.ResponseBuilder _ _ builder -> Just
          . Text.encodeUtf8
          . Text.pack
          . show
          . show
          . Crypto.hashWith Crypto.SHA256
          . LazyByteString.toStrict
          $ Builder.toLazyByteString builder
        _ -> Nothing
    in respond $ if actual == expected
      then Wai.responseLBS Http.notModified304 [] LazyByteString.empty
      else case actual of
        Nothing -> response
        Just etag -> Wai.mapResponseHeaders ((Http.hETag, etag) :) response


application :: Wai.Application
application request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of

    ("GET", []) -> respond . htmlResponse . Lucid.doctypehtml_ $ do
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
        Lucid.div_ [Lucid.class_ "gray pa3 tc"]
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
  contents <- LazyByteString.readFile path
  pure $ Wai.responseLBS Http.ok200 [(Http.hContentType, mime)] contents


htmlResponse :: Lucid.Html a -> Wai.Response
htmlResponse =
  Wai.responseLBS Http.ok200 [(Http.hContentType, "text/html; charset=utf-8")]
    . Lucid.renderBS


statusResponse :: Http.Status -> Wai.Response
statusResponse status = textResponse status $ Text.unwords
  [ Text.pack . show $ Http.statusCode status
  , Text.decodeUtf8With Text.lenientDecode $ Http.statusMessage status
  ]

textResponse :: Http.Status -> Text.Text -> Wai.Response
textResponse status =
  Wai.responseLBS status [(Http.hContentType, "text/plain; charset=utf-8")]
    . LazyByteString.fromStrict
    . Text.encodeUtf8
