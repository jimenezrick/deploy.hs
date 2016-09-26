module Cook.Catalog.Arch.Rootfs (
    buildRootfs
  ) where

import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import qualified Data.Text as T

import Cook.Recipe
import Cook.Recipe.Util

buildRootfs :: FilePath -> Recipe FilePath
buildRootfs path = withRecipeName "Arch.Rootfs.BuildRootfs" $ do
    liftIO $ createDirectory path
    runProc "pacstrap" ["-i", "-c", "-d", path, "--noconfirm", "base"]
    enablePts0 $ path </> "etc/securetty"
    return path

enablePts0 :: FilePath -> Recipe ()
enablePts0 securetty = withRecipeName "EnablePts0" $ do
    withFileContent securetty $ T.unlines . insertPts0 . T.lines
  where insertPts0 ls = init ls ++ ["pts/0", "", last ls]
