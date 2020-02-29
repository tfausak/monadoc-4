module Main
  ( main
  )
where

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as Text
import qualified Language.Haskell.Brittany as Brittany
import qualified Language.Haskell.HLint4 as Hlint
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do
  files <- Hspec.runIO $ fmap List.sort getHaskellSourceFiles

  Hspec.describe "brittany" . Hspec.parallel $ do
    config <- Hspec.runIO getBrittanyConfig
    mapM_ (brittany config) files

  Hspec.describe "hlint" . Hspec.parallel $ mapM_ hlint files

brittany :: Brittany.Config -> FilePath -> Hspec.Spec
brittany config file = Hspec.it file $ do
  actual <- Text.readFile file
  result <- Brittany.parsePrintModule config actual
  case result of
    Left brittanyError ->
      fail . unlines $ fmap showBrittanyError brittanyError
    Right expected -> expected `Hspec.shouldBe` actual

hlint :: FilePath -> Hspec.Spec
hlint file = Hspec.it file $ do
  ideas <- Hlint.hlint ["--quiet", file]
  ideas `Hspec.shouldSatisfy` null

showBrittanyError :: Brittany.BrittanyError -> String
showBrittanyError brittanyError = case brittanyError of
  Brittany.ErrorInput x -> "ErrorInput " <> show x
  Brittany.ErrorMacroConfig x y ->
    unwords ["ErrorMacroConfig", show x, show y]
  Brittany.ErrorOutputCheck -> "ErrorOutputCheck"
  Brittany.ErrorUnknownNode x _ ->
    unwords ["ErrorUnknownNode", show x, "<ast>"]
  Brittany.ErrorUnusedComment x -> "ErrorUnusedComment " <> show x
  Brittany.LayoutWarning x -> "LayoutWarning " <> show x

getBrittanyConfig :: IO Brittany.Config
getBrittanyConfig = do
  maybeLocalConfig <- Brittany.findLocalConfigPath "."
  maybeConfig <-
    MaybeT.runMaybeT
    . Brittany.readConfigsWithUserConfig mempty
    $ Maybe.maybeToList maybeLocalConfig
  pure $ Maybe.fromMaybe Brittany.staticDefaultConfig maybeConfig

getHaskellSourceFiles :: IO [FilePath]
getHaskellSourceFiles = getHaskellFiles sourceDirectory

sourceDirectory :: FilePath
sourceDirectory = "source"

getHaskellFiles :: FilePath -> IO [FilePath]
getHaskellFiles = fmap (filter hasHaskellExtension) . listDirectoryRecursively

hasHaskellExtension :: FilePath -> Bool
hasHaskellExtension = FilePath.isExtensionOf "hs"

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively directory = do
  entries <- Directory.listDirectory directory
  concatMapM (listSubdirectoryRecursively directory) entries

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

listSubdirectoryRecursively :: FilePath -> FilePath -> IO [FilePath]
listSubdirectoryRecursively directory entry = do
  let path = FilePath.combine directory entry
  isDirectory <- Directory.doesDirectoryExist path
  if isDirectory then listDirectoryRecursively path else pure [path]
