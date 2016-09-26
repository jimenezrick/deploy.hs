import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Text.Printf

import Cook.Recipe
import Cook.Recipe.Bootstrap
import Cook.Catalog.Arch.Rootfs
import Cook.Catalog.Systemd.Container
import Cook.Catalog.Cjdns

main :: IO ()
main = runRecipe $ do
    args <- liftIO $ getArgs
    case args of
        "container":name:extraPkgs -> do
            void $ buildRootfs name extraPkgs
            tarball <- tarRootfs name
            liftIO $ printf "Container created: %s\n" tarball
        ["cjdns", nodeConf] -> do
            bootstrapCook
            requireCjdns nodeConf
        _ -> error "Invalid arguments"

-- XXX: Copy everything to /cook/ ?
