{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- |
-- Module      : Properties.Conduit.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Conduit.Graphite (
      graphiteProperties
    ) where

import Control.Applicative                  hiding (empty)
import Data.Maybe
import Numbers.Conduit
import Numbers.Types
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [ testGroup "flush event"
        [ testProperty "encodes prefix" prop_encodes_prefix
        , testProperty "encodes key" prop_encodes_key
        , testProperty "encodes value" prop_encodes_value
        ]
    , testGroup "other events"
         [ testProperty "are ignored" prop_ignores_non_flush_event
         ]
    ]

prop_encodes_prefix :: FlushEvent -> Bool
prop_encodes_prefix evt = inputPrefix evt == outputPrefix evt

prop_encodes_key :: FlushEvent -> Bool
prop_encodes_key evt = inputKey evt == outputKey evt

prop_encodes_value :: FlushEvent -> Bool
prop_encodes_value evt = kindaClose (inputValue evt) (outputValue evt)

prop_ignores_non_flush_event :: NonFlushEvent -> Bool
prop_ignores_non_flush_event (NonFlushEvent _ bs) = null bs

data NonFlushEvent = NonFlushEvent Event [BS.ByteString]
    deriving (Show)

instance Arbitrary NonFlushEvent where
    arbitrary = do
        e  <- suchThat arbitrary p
        bs <- conduitResult e $ graphite ""
        return $ NonFlushEvent e bs
      where
        p (Flush _ _) = False
        p _           = True

data FlushEvent = FlushEvent
    { inputPrefix  :: String
    , inputKey     :: Key
    , inputTime    :: Time
    , inputValue   :: Double
    , outputPrefix :: String
    , outputKey    :: Key
    , outputTime   :: Time
    , outputValue  :: Double
    } deriving (Show)

instance Arbitrary FlushEvent where
    arbitrary = do
        SafeStr ip  <- arbitrary
        it          <- arbitrary
        p@(P ik iv) <- arbitrary
        bs          <- conduitResult (Flush it p) (graphite ip)
        let (op, ok, ot, ov) = parse bs
        return $ FlushEvent
            { inputPrefix  = ip
            , inputKey     = ik
            , inputTime    = it
            , inputValue   = iv
            , outputPrefix = op
            , outputKey    = ok
            , outputTime   = ot
            , outputValue  = ov
            }

parse :: [BS.ByteString] -> (String, Key, Time, Double)
parse = fromJust . decode format . BS.concat
  where
    format = do
        p <- PC.takeTill (== '.') <* PC.char '.'
        k <- PC.takeTill (== ' ') <* PC.char ' '
        v <- PC.double <* PC.char ' '
        t <- PC.decimal
        return (BS.unpack p, Key k, Time t, v)