module Cook.Provider
    ( module Cook.Provider.PkgManager

    , Provider
    , pkgManager

    , getProvider
    ) where

import Control.Lens

import Cook.Provider.PkgManager hiding (Provider)
import Cook.Recipe
import Cook.Facts

import qualified Cook.Catalog.Arch.Pacman as A
import qualified Cook.Provider.PkgManager as P

data Provider f = Provider
    { _pkgManager :: P.Provider f
    }

makeLenses ''Provider

chooseProvider :: Facts f -> Provider f
chooseProvider facts = case facts^.systemFacts.osRelease.distro of
                           Arch -> Provider A.provider

getProvider :: Recipe f (Provider f)
getProvider = chooseProvider <$> getFacts
