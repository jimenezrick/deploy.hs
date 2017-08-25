{-# LANGUAGE RecordWildCards #-}
module Cook.Catalog.Debian.Debootstrap where

import Data.Semigroup ((<>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import System.Directory
import System.FilePath ((</>))

import Cook.Catalog.Debian.Suite (showSuite, Suite)
import Cook.Recipe (Recipe, runRead, withRecipeName, Step, proc)
import Control.Monad.IO.Class (liftIO)

data Variant = Minbase | Buildd | Fakechroot | Scratchbox
  deriving Show

data Options = Options
  { downloadOnly  :: Bool
  , include       :: [String]
  , exclude       :: [String]
  , variant       :: Variant
  } deriving Show

defaults = Options
  { downloadOnly = False
  , include = []
  , exclude = []
  , variant = Minbase
  }

type Mirror = String

debootstrap :: Options -> Suite -> FilePath -> Maybe Mirror -> Step
debootstrap Options {..} suite path mirror = proc "debootstrap" (options <> args)
  where args    = catMaybes [ Just . showSuite $ suite, Just path, mirror ]
        options = catMaybes [ if downloadOnly then Just "--download-only" else Nothing
                            , if not . null $ include
                                then Just ("--include=" <> intercalate "," include)
                                else Nothing
                            , if not . null $ exclude
                                then Just ("--exclude=" <> intercalate "," exclude)
                                else Nothing
                            , Just . mappend "--variant=" . map toLower . show $ variant
                            ]
