{-|
Module      : Main
Description : Main entrypoint for catnplus
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module Main (main) where

import           CatNPlus
import           Options.Applicative
import           VersionInfo

data Command = ShowFiles [FilePath] | ShowVersion

productVersion :: String
productVersion = "catnplus " ++ fullVersionString

commandParser :: Parser Command
commandParser = filePathsParser <|> versionParser

filePathsParser :: Parser Command
filePathsParser = ShowFiles <$> some (argument str (metavar "FILES..."))

versionParser :: Parser Command
versionParser = flag' ShowVersion (short 'v' <> long "version" <> help "Show version")

main :: IO ()
main = parseOptions >>= runCommand
    where
        parseOptions = execParser $ info
            (helper <*> commandParser)
            (fullDesc <> progDesc "Display text files with line numbers" <> header productVersion)
        runCommand (ShowFiles paths) = runApp paths
        runCommand ShowVersion = putStrLn productVersion
