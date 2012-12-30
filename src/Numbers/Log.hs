-- |
-- Module      : Numbers.Log
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Log
    (
      module Generics.Deriving.GBuild

    , infoL
    , errorL
    , logL

    , defaultLogger
    , newLogger
    ) where

import Generics.Deriving.GBuild
import System.IO
import System.IO.Unsafe
import System.Log.FastLogger


infoL :: GBuild a => a -> IO ()
infoL = errorL

errorL :: GBuild a => a -> IO ()
errorL = logL defaultLogger

logL :: GBuild a => Logger -> a -> IO ()
logL logger s = loggerPutBuilder logger . gbuild $ s <&> "\n"

defaultLogger :: Logger
defaultLogger = unsafePerformIO $ mkLogger True stdout
{-# NOINLINE defaultLogger #-}

newLogger :: GBuild a => FilePath -> IO (a -> IO ())
newLogger path = do
    h <- case path of
             "stdout" -> return stdout
             "stderr" -> return stderr
             _        -> openFile path AppendMode
    logL `fmap` mkLogger True h
