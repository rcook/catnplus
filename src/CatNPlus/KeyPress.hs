{-|
Module      : CatNPlus.KeyPress
Description : Key press detections
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CatNPlus.KeyPress (waitForKeyPress) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
#ifdef mingw32_HOST_OS
import           Data.Char
import           Foreign.C.Types
#endif
import           System.Console.ANSI
import           System.IO

data HandleState = HandleState BufferMode Bool

-- Hack based on http://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline
getHiddenChar :: IO Char
#ifdef mingw32_HOST_OS
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch" c_getch :: IO CInt
#else
getHiddenChar = getChar
#endif

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

prompt :: String
prompt = ": "

waitForKeyPress :: [Char] -> IO Char
waitForKeyPress cs = bracketHandle stdin $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    bracket_
        (putStr prompt >> hFlush stdout)
        (cursorBackward (length prompt) >> clearFromCursorToLineEnd) $ do
            Left c <- runEitherT $ forever $ do
                c <- lift getHiddenChar
                when (c `elem` cs) (left c)
            return c
