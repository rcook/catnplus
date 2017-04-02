{-|
Module      : CatNPlus.KeyPress
Description : Key press detections
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module CatNPlus.KeyPress (waitForKeyPressOneOf) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           System.Console.ANSI
import           System.IO
import           System.IO.HiddenChar

prompt :: String
prompt = ": "

waitForKeyPressOneOf :: [Char] -> IO Char
waitForKeyPressOneOf cs = bracket_
    (putStr prompt >> hFlush stdout)
    (cursorBackward (length prompt) >> clearFromCursorToLineEnd) $ do
        Left c <- runEitherT $ forever $ do
            c <- lift getHiddenChar
            when (c `elem` cs) (left c)
        return c
