module Cook.Catalog.Debian.Apt (
    installPackages
  , updatePackages
  , upgradePackages
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe

aptGet :: NonEmpty String -> Step
aptGet args = procEnv "apt-get" args' env
  where env = Just [("DEBIAN_FRONTEND", "noninteractive")]
        args' = toList $ ["-q", "-y"] <> args

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $ do
  run $ aptGet $ ["install"] <> pkgs

updatePackages :: Recipe ()
updatePackages = withRecipeName "Debian.Apt.UpdatePackages" $ do
  run $ aptGet $ ["update"]

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
  run $ aptGet $ ["upgrade"]
