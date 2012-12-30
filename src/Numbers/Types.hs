{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Numbers.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Types
    ( -- * Exported Types
      Dec(..)
    , Key(..)
    , Metric(..)
    , Point(..)
    , Time(..)
    , Uri(..)

      -- * Functions
    , aggregate
    , calculate
    , currentTime
    , decode
    , keyParser
    , lineParser
    , nl
    , uriParser
    , zero
    ) where

import           Blaze.ByteString.Builder
import           Control.Applicative        hiding (empty)
import           Control.Arrow              (first, (***))
import           Control.Monad
import           Data.Aeson                 (ToJSON (..))
import           Data.Attoparsec.ByteString
import           Data.List                  hiding (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Statistics.Function        (sort)
import           Statistics.Sample
import           Text.Regex.PCRE            hiding (match)

import qualified Data.Attoparsec.Char8      as PC
import qualified Data.ByteString.Char8      as BS
import qualified Data.Set                   as S
import qualified Data.Vector                as V

import           Numbers.Log

newtype Time = Time Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic)

instance GBuild Time where gbuild (Time t) = gbuild t

currentTime :: IO Time
currentTime = (Time . truncate) `fmap` getPOSIXTime

data Uri = File { _path :: BS.ByteString }
         | Tcp  { _host :: BS.ByteString, _port :: Int }
         | Udp  { _host :: BS.ByteString, _port :: Int }
  deriving (Eq, Show, Generic)

instance GBuild Uri where
    gbuild x@(File f)  = scheme x <&> f
    gbuild x@(Tcp h p) = scheme x <&> h <&> ':' <&> p
    gbuild x@(Udp h p) = scheme x <&> h <&> ':' <&> p

scheme :: Uri -> BS.ByteString
scheme (File _)  = "file://"
scheme (Tcp _ _) = "tcp://"
scheme (Udp _ _) = "udp://"

instance Read Uri where
    readsPrec _ a = return (fromJust . decode uriParser $ BS.pack a, "")

instance ToJSON Uri where
    toJSON = toJSON . toByteString . gbuild

decode :: Parser a -> BS.ByteString -> Maybe a
decode p bstr = maybeResult $ feed (parse p bstr) BS.empty

uriParser :: Parser Uri
uriParser = do
    s <- PC.takeTill (== ':') <* string "://"
    case BS.unpack s of
        "file" -> File <$> PC.takeByteString
        "tcp"  -> Tcp  <$> host <*> port
        "udp"  -> Udp  <$> host <*> port
        _      -> error "Unrecognized uri scheme"
  where
    host = PC.takeTill (== ':') <* PC.char ':'
    port = PC.decimal :: Parser Int

newtype Key = Key BS.ByteString
    deriving (Eq, Ord, Show, Generic)

instance Monoid Key where
    (Key a) `mappend` (Key b) = Key $ BS.concat [a, ".", b]
    mempty = Key mempty

instance GBuild Key where gbuild (Key k) = gbuild k

instance ToJSON Key where
    toJSON (Key k) = toJSON k

keyParser :: Parser Key
keyParser = do
    k <- PC.takeTill (== ':')
    return $! Key (strip k)

strip :: BS.ByteString -> BS.ByteString
strip s = foldl (flip $ uncurry replace) s unsafe

unsafe :: [(Regex, BS.ByteString)]
unsafe = map (first makeRegex . join (***) BS.pack) rs
  where
    rs = [ ("\\s+", "_")
         , ("\\/", "-")
         , ("[^a-zA-Z_\\-0-9\\.]", "")
         ]

replace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace regex rep = f
  where
    f s = case match regex s of
        Just (a, _, c) -> a `BS.append` rep `BS.append` f c
        _              -> s

match :: Regex
      -> BS.ByteString
      -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
match = matchM

newtype Dec = Dec { unDec :: Double }
    deriving ( Enum
             , Floating
             , Fractional
             , Generic
             , Num
             , Read
             , Real
             , RealFloat
             , RealFrac
             , Show
             )

-- necessary for Set
instance Eq Dec  where (Dec a) == (Dec b) = a == b
instance Ord Dec where compare (Dec a) (Dec b) = a `compare` b

instance GBuild Dec where
    gbuildbPrec _ (Dec d) = buildFFloat (Just 1) d

data Metric = Counter !Dec
            | Gauge   !Dec
            | Timer   !(V.Vector Dec)
            | Set     !(S.Set Dec)
    deriving (Eq, Ord, Show, Generic)

instance GBuild Metric where
    gbuild m = case m of
                  Counter v -> v <&> b "|c"
                  Gauge   v -> v <&> b "|g"
                  Timer  vs -> g (<&> b "|ms") $ V.toList vs
                  Set    ss -> g (<&> b "|s")
                               $ S.toAscList . S.map (toByteString . gbuild)
                               $ ss
        where
            g h = mconcat . intersperse (gbuild ':') . map h
            b = BS.pack

instance GBuild (Key, Metric) where gbuild (k, m) = k <&> ':' <&> m

lineParser :: Parser (Key, Metric)
lineParser = do
     k  <- keyParser
     ms <- metricsParser
     return $! (k, foldl1 (flip aggregate . Just) ms)

metricsParser :: Parser [Metric]
metricsParser = many1 $ do
    _ <- optional $ PC.char ':'
    v <- Dec `fmap` value
    t <- type'
    r <- optional (Dec `fmap` sample)
    return $! case t of
        'g' -> Gauge   v
        'm' -> Timer   $ V.singleton v
        's' -> Set     $ S.singleton v
        _   -> Counter $ maybe v (\n -> v * (1 / n)) r -- ^ Div by zero
  where
    value  = PC.double <* PC.char '|'
    sample = PC.char '|' *> PC.char '@' *> PC.double
    type'  = PC.char 'c'
         <|> PC.char 'g'
         <|> PC.char 'm' <* PC.char 's'
         <|> PC.char 's'

zero :: Metric -> Bool
zero (Counter 0) = True
zero (Gauge   0) = True
zero (Timer  ns) = V.null ns
zero (Set    ss) = S.null ss
zero _           = False

aggregate :: Metric -> Maybe Metric -> Metric
aggregate a Nothing  = a
aggregate a (Just b) = b `f` a
  where
    f (Counter x) (Counter y) = Counter $ x + y
    f (Timer   x) (Timer   y) = Timer   $ x V.++ y
    f (Set     x) (Set     y) = Set     $ x `S.union` y
    f _           _           = b

data Point = P !Key !Dec
    deriving (Show, Generic)

instance GBuild Point where gbuild (P k d) = k <&> ' ' <&> d

calculate :: [Int] -> Int -> Key -> Metric -> [Point]
calculate _  n k (Counter v) =
    [ P (Key "counters" <> k) (v / (fromIntegral n / 1000))
    , P (Key "counters" <> k <> Key "count") v
    ]
calculate _  _ k (Gauge v) =
    [ P (Key "gauges" <> k) v ]
calculate _  _ k (Set ss) =
    [ P (Key "sets" <> k <> Key "count") (fromIntegral $ S.size ss) ]
calculate qs _ k (Timer vs) = concatMap (quantile k xs) qs <>
    [ P (Key "timers" <> k <> Key "std")   $ Dec . stdDev . V.map unDec $ xs
    , P (Key "timers" <> k <> Key "upper") $ V.last xs
    , P (Key "timers" <> k <> Key "lower") $ V.head xs
    , P (Key "timers" <> k <> Key "count") . fromIntegral $ V.length xs
    , P (Key "timers" <> k <> Key "sum")   $ V.sum xs
    , P (Key "timers" <> k <> Key "mean")  $ Dec . mean . V.map unDec $ xs
    ]
  where
    xs = sort vs

quantile :: Key -> V.Vector Dec -> Int -> [Point]
quantile k xs q =
    [ P (Key "timers" <> k <> a "mean_")  $ Dec . mean . V.map unDec $ ys
    , P (Key "timers" <> k <> a "upper_") $ V.last ys
    , P (Key "timers" <> k <> a "sum_")   $ V.sum ys
    ]
  where
    a  = Key . (`BS.append` BS.pack (show q))
    n  = round $ fromIntegral q / 100 * (fromIntegral $ V.length xs :: Double)
    ys = V.take n xs

nl :: BS.ByteString
nl = BS.pack "\n"
