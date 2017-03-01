{-|
Module      : VersionInfo
Description : Version information for catnplus
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE TemplateHaskell #-}

module VersionInfo (fullVersionString) where

import           Data.Version
import           Distribution.VcsRevision.Git
import           Language.Haskell.TH.Syntax
import           Paths_catnplus

gitVersionString :: String
gitVersionString = $(do
    v <- qRunIO getRevision
    lift $ case v of
        Nothing -> []
        Just (commit, True)  -> commit ++ " (locally modified)"
        Just (commit, False) -> commit)

fullVersionString :: String
fullVersionString = case gitVersionString of
    [] -> showVersion version
    v -> showVersion version ++ "." ++ v
