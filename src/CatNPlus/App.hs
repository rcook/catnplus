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
import           System.Directory
import           System.Environment
import           Text.Printf

pageLength :: Int
pageLength = 5

withSGR :: [ANSI.SGR] -> IO a -> IO a
withSGR sgrs = bracket_
    (ANSI.setSGR sgrs)
    (ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])

runApp :: IO ()
runApp = do
    paths <- getArgs
    void $ runEitherT $ forM_ paths $ \p -> do
        content <- liftIO $ do
            path <- canonicalizePath p
            withSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green] $
                putStrLn path
            readFile path
        runEitherT $ forM_ (zip [1..] (lines content)) $ \(n, line) -> do
            liftIO $ do
                withSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow] $
                    putStr (printf "%6d" n)
                putStrLn $ "  " ++ line

            when (n `mod` pageLength == 0) $ do
                shouldContinue <- liftIO $ do
                    c <- waitForKeyPress ['Q', 'q', ' ']
                    return $ c /= 'Q' && c/= 'q'
                when (not shouldContinue) $ lift $ left ()
