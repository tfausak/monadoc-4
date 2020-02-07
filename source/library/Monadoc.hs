module Monadoc (main) where

import qualified Data.Version as Version
import qualified Paths_monadoc as Package


main :: IO ()
main = putStrLn $ "monadoc-" <> Version.showVersion Package.version
