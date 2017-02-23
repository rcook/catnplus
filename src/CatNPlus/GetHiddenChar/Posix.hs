{-|
Module      : CatNPlus
Description : Posix implementation of GetHiddenChar
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module CatNPlus.GetHiddenChar.Posix (getHiddenChar) where

import           Control.Exception
import           System.IO

data HandleState = HandleState BufferMode Bool

hGetState :: Handle -> IO HandleState
hGetState h = do
    bufferMode <- hGetBuffering h
    isEcho <- hGetEcho h
    return $ HandleState bufferMode isEcho

hSetState :: Handle -> HandleState -> IO ()
hSetState h (HandleState mode isEcho) = do
    hSetEcho h isEcho
    hSetBuffering h mode

bracketHandle :: Handle -> (IO a -> IO a)
bracketHandle h action = bracket (hGetState h) (hSetState h) (const action)

getHiddenChar :: IO Char
getHiddenChar = bracketHandle stdin $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    getChar
