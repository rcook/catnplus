{-|
Module      : CatNPlus.App
Description : Application entrypoint for catnplus
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module CatNPlus.App (runApp) where

import           CatNPlus.KeyPress
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as TS
import           System.Directory
import           System.Environment
import           System.IO
import           Text.Printf

withSGR :: [ANSI.SGR] -> IO a -> IO a
withSGR sgrs = bracket_
    (ANSI.setSGR sgrs)
    (ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])

withSGRCond :: Bool -> [ANSI.SGR] -> IO a -> IO a
withSGRCond True sgrs action = withSGR sgrs action
withSGRCond False _ action = action

getPageLength :: IO Int
getPageLength = do
    mbWindow <- TS.size
    case mbWindow of
        Nothing -> return 20
        Just (TS.Window h _) -> return $ h - 2

runApp :: IO ()
runApp = do
    paths <- getArgs
    pageLength <- getPageLength
    isTerminal <- hIsTerminalDevice stdout
    void $ runEitherT $ forM_ paths $ \p -> do
        content <- liftIO $ do
            path <- canonicalizePath p
            withSGRCond isTerminal [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green] $
                putStrLn path
            readFile path
        runEitherT $ forM_ (zip [1..] (lines content)) $ \(n, line) -> do
            liftIO $ do
                withSGRCond isTerminal [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow] $
                    putStr (printf "%6d" n)
                putStrLn $ "  " ++ line

            when (isTerminal && n `mod` pageLength == 0) $ do
                shouldContinue <- liftIO $ do
                    c <- waitForKeyPressOneOf ['Q', 'q', ' ']
                    return $ c /= 'Q' && c/= 'q'
                when (not shouldContinue) $ lift $ left ()
