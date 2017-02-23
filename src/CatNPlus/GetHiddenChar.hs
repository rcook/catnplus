{-|
Module      : CatNPlus
Description : Umbrella module for GetHiddenChar
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE CPP #-}

module CatNPlus.GetHiddenChar (getHiddenChar) where

#if defined(OS_Linux) || defined(OS_macOS)
import           CatNPlus.GetHiddenChar.Posix
#elif defined(OS_Windows)
import           CatNPlus.GetHiddenChar.Windows
#else
#error Unsupported platform
#endif
