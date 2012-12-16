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

module Numbers.Log (
      Loggable(..)
    , sbuild

    , infoL
    , infoL'
    , errorL
    , errorL'
    , logL
    , logL'

    , defaultLogger
    , newLogger
    ) where

import Blaze.ByteString.Builder
import Control.Monad
import Data.List
import Data.Monoid
import Numeric                  (showFFloat)
import System.Log.FastLogger
import System.IO
import System.IO.Unsafe

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector           as V


class Loggable a where
    build :: Loggable a => a -> Builder
    (&&&) :: (Loggable a, Loggable b) => a -> b -> Builder
    (<&&) :: Loggable a => String -> a -> Builder
    (&&>) :: Loggable a => a -> String -> Builder

    infixr 7 &&&
    infixr 9 <&&
    infixr 8 &&>

    a &&& b = build a <> build b
    a <&& b = sbuild a &&& b
    a &&> b = a &&& sbuild b

sbuild :: String -> Builder
sbuild = build . BS.pack

instance Loggable Builder where
    build = id

instance Loggable BS.ByteString where
    build = copyByteString

instance Loggable Int where
    build = sbuild . show

instance Loggable Double where
    build n = sbuild $ showFFloat (Just 1) n ""

instance Loggable a => Loggable (Maybe a) where
    build (Just x) = build x
    build Nothing  = mempty

instance Loggable a => Loggable [a] where
    build = mconcat . intersperse (sbuild ", ") . map build

instance Loggable (V.Vector Double) where
    build = build . V.toList


infoL :: Loggable a => a -> IO ()
infoL = errorL

infoL' :: String -> IO ()
infoL' = infoL . sbuild

errorL :: Loggable a => a -> IO ()
errorL = logL defaultLogger

errorL' :: String -> IO ()
errorL' = errorL . sbuild

logL :: Loggable a => Logger -> a -> IO ()
logL logger s = loggerPutBuilder logger $ s &&> "\n"

logL' :: Logger -> String -> IO ()
logL' logger s = logL logger (sbuild s)

defaultLogger :: Logger
defaultLogger = unsafePerformIO $ mkLogger True stdout
{-# NOINLINE defaultLogger #-}

newLogger :: Loggable a => FilePath -> IO (a -> IO ())
newLogger path = do
    h <- case path of
             "stdout" -> return stdout
             "stderr" -> return stderr
             _        -> openFile path AppendMode
    logL `liftM` mkLogger True h
