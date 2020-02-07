{-# LANGUAGE OverloadedStrings #-}

module Monadoc (main) where

import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_monadoc as Package


main :: IO ()
main = do
  say "starting up"
  Warp.runSettings settings application


say :: Text.Text -> IO ()
say message = do
  now <- Time.getCurrentTime
  Text.putStrLn $ formatTime now <> " " <> message


formatTime :: Time.UTCTime -> Text.Text
formatTime =
  Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"


settings :: Warp.Settings
settings = Warp.defaultSettings
  & Warp.setBeforeMainLoop beforeMainLoop
  & Warp.setHost host
  & Warp.setOnExceptionResponse onExceptionResponse
  & Warp.setPort port
  & Warp.setServerName serverName


beforeMainLoop :: IO ()
beforeMainLoop = say $ Text.unwords
  ["listening on", Text.pack $ show host, "port", Text.pack $ show port]


host :: Warp.HostPreference
host = "*"


onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ =
  Wai.responseLBS Http.internalServerError500 [] LazyByteString.empty


port :: Warp.Port
port = 8080


serverName :: ByteString.ByteString
serverName =
  let
    version = Text.encodeUtf8 . Text.pack $ Version.showVersion Package.version
  in "monadoc-" <> version


application :: Wai.Application
application _request respond = respond
  $ Wai.responseLBS Http.ok200 [] LazyByteString.empty
