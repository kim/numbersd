-- |
-- Module      : Numbers.Whisper
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper (
    -- * Opaque
      Whisper
    , newWhisper

    -- * Operations
    , insert

    -- * Formatters
    , json
    , text
    ) where

import Blaze.ByteString.Builder        (Builder, copyLazyByteString)
import Control.Arrow                   (second)
import Control.Monad                   (liftM)
import Data.Aeson               hiding (json)
import Data.Text.Encoding              (decodeUtf8)
import Numbers.Types
import Numbers.Whisper.Series          (Resolution, Series, Step)

import qualified Control.Concurrent.STM.Map as M
import qualified Data.ByteString.Char8      as BS
import qualified Numbers.Whisper.Series     as S

data Whisper = Whisper
    { quant :: [Int]
    , pref  :: BS.ByteString
    , res   :: Resolution
    , step  :: Step
    , db    :: M.Map BS.ByteString Series
    }

newWhisper :: [Int]         -- ^ Quantiles
           -> Int           -- ^ Resolution
           -> Int           -- ^ Step
           -> BS.ByteString -- ^ Prefix
           -> IO Whisper
newWhisper qs res step pref =
    Whisper qs pref (res `div` step) step `liftM` M.empty
-- ^ Investigate implications of div absolute rounding torwards zero

insert :: Key -> Metric -> Time -> Whisper -> IO ()
insert key m ts w@Whisper{..} = mapM_ (\(k, v) -> update k ts v w) f
  where
    f = map (flatten pref key) (calculate quant res m)

json :: Time -> Time -> Whisper -> IO Builder
json from to w =
    (copyLazyByteString . encode . object . map f) `liftM` fetch from to w
  where
    f (k, s) = decodeUtf8 k .= toJSON s

text :: Time -> Time -> Whisper -> IO Builder
text from to w = (build . map f) `liftM` fetch from to w
  where
    f (k, s) = k &&> "," &&& s &&> "\n"

update :: BS.ByteString -> Time -> Double -> Whisper -> IO ()
update key ts val Whisper{..} = M.update key f db
  where
    f = return . maybe (S.create res step ts val) (S.update ts val)

fetch :: Time -> Time -> Whisper -> IO [(BS.ByteString, Series)]
fetch from to w = map (second (S.fetch from to)) `liftM` M.toList (db w)
