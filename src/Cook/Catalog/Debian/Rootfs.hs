module Cook.Catalog.Debian.Rootfs (
    buildRootfs
  , buildRootfs'
  , buildRootfsFromCache
  ) where

import Cook.Catalog.Debian.Debootstrap (debootstrap, defaults, include, Options, variant, Variant(Minbase))
import Cook.Catalog.Debian.Suite (showSuite, stable, Suite)
import Cook.Recipe (Recipe, run, withRecipeName, runProc)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), addTrailingPathSeparator)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, void)
import Data.List (isSuffixOf)
import Data.Monoid ((<>))

type RootFs = FilePath
type Package = String

defaultPackages :: [Package]
defaultPackages = [ "dbus"
                  , "dialog"
                  , "ifupdown"
                  , "init"
                  , "iproute"
                  , "isc-dhcp-client"
                  , "locales"
                  , "net-tools"
                  , "netbase"
                  , "openssh-server"
                  , "sudo"
                  ]

runDebootstrap :: Suite -> RootFs -> [Package] -> Recipe ()
runDebootstrap suite path extraPkgs =
  run (debootstrap defaults{include=extraPkgs} suite path Nothing)

buildRootfs :: Suite -> RootFs -> [Package] -> Recipe FilePath
buildRootfs suite path extraPkgs = withRecipeName "Debian.Rootfs.BuildRootfs" $ do
  runDebootstrap suite path extraPkgs
  return path

buildRootfs' :: Suite -> RootFs -> Recipe FilePath
buildRootfs' suite path = buildRootfs suite path defaultPackages

type CacheDir = FilePath

buildRootfsFromCache :: CacheDir -> Suite -> RootFs -> Recipe RootFs
buildRootfsFromCache cache suite path = withRecipeName "Debian.Rootfs.BuildRootfsFromCache" $ do
  let suiteCache = cache </> (addTrailingPathSeparator . showSuite $ suite)
  liftIO $ createDirectoryIfMissing True cache
  suiteCached <- liftIO $ doesDirectoryExist suiteCache
  unless suiteCached (void (runDebootstrap suite suiteCache defaultPackages))
  runProc "rsync" ["-H", "-a", suiteCache, path]
  return path
