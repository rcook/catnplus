{-|
Module      : CatNPlus
Description : Windows implementation of GetHiddenChar
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module CatNPlus.GetHiddenChar.Windows (getHiddenChar) where

import           Data.Char
import           Foreign.C.Types

foreign import ccall unsafe "conio.h getch" c_getch :: IO CInt

-- Hack based on http://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline
getHiddenChar :: IO Char
getHiddenChar = fmap (chr.fromEnum) c_getch
