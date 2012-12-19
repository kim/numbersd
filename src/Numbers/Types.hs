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

module Numbers.Types (
    -- * Exported Types
      Time(..)
    , Uri(..)
    , Key(..)
    , Metric(..)
    , Point(..)

    -- * Functions
    , lineParser
    , keyParser
    , uriParser
    , decode
    , currentTime
    , zero
    , aggregate
    , calculate
    ) where

import Blaze.ByteString.Builder
import Control.Arrow                     ((***), first)
import Control.Applicative        hiding (empty)
import Control.Monad
import Data.Aeson                        (ToJSON(..))
import Data.Attoparsec.ByteString
import Data.List                  hiding (sort)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Time.Clock.POSIX
import Numbers.Log
import Statistics.Function               (sort)
import Statistics.Sample
import Text.Regex.PCRE            hiding (match)

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import qualified Data.Text.Encoding    as TE
import qualified Data.Vector           as V


newtype Time = Time Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Loggable Time where
    build (Time n) = build n

currentTime :: IO Time
currentTime = (Time . truncate) `liftM` getPOSIXTime

data Uri = File { _path :: BS.ByteString }
         | Tcp  { _host :: BS.ByteString, _port :: Int }
         | Udp  { _host :: BS.ByteString, _port :: Int }
           deriving (Eq, Show)

instance Read Uri where
    readsPrec _ a = return (fromJust . decode uriParser $ BS.pack a, "")

instance IsString Uri where
    fromString = fromJust . decode uriParser . BS.pack

instance ToJSON Uri where
    toJSON = toJSON . TE.decodeUtf8 . toByteString . build

decode :: Parser a -> BS.ByteString -> Maybe a
decode p bstr = maybeResult $ feed (parse p bstr) BS.empty

instance Loggable Uri where
    build (File f)  = "file://" <&& f
    build (Tcp h p) = "tcp://"  <&& h &&& ":" <&& p
    build (Udp h p) = "udp://"  <&& h &&& ":" <&& p

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
    deriving (Eq, Ord, Show)

instance IsString Key where
    fromString = Key . BS.pack

instance Monoid Key where
    (Key a) `mappend` (Key b) = Key $ BS.concat [a, ".", b]
    mempty = Key mempty

instance Loggable Key where
    build (Key k) = build k

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

data Metric = Counter !Double
            | Gauge   !Double
            | Timer   !(V.Vector Double)
            | Set     !(S.Set Double)
              deriving (Eq, Ord, Show)

instance Loggable Metric where
    build m = case m of
                  Counter v -> v &&> "|c"
                  Gauge   v -> v &&> "|g"
                  Timer  vs -> g (&&> "|ms") $ V.toList vs
                  Set    ss -> g (&&>  "|s") $ S.toAscList ss
        where
            g h = mconcat . intersperse (sbuild ":") . map h

instance Loggable (Key, Metric) where
    build (k, m) = k &&& (sbuild ":") &&& build m

lineParser :: Parser (Key, Metric)
lineParser = do
     k  <- keyParser
     ms <- metricsParser
     return $! (k, foldl1 (flip aggregate . Just) ms)

metricsParser :: Parser [Metric]
metricsParser = many1 $ do
    _ <- optional $ PC.char ':'
    v <- value
    t <- type'
    r <- optional sample
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

data Point = P !Key !Double
    deriving (Show)

instance Loggable Point where
    build (P k v) = k &&> " " &&& v

calculate :: [Int] -> Int -> Key -> Metric -> [Point]
calculate _  n k (Counter v) =
    [ P ("counters" <> k) (v / (fromIntegral n / 1000))
    , P ("counters" <> k <> "count") v
    ]
calculate _  _ k (Gauge v) =
    [ P ("gauges" <> k) v ]
calculate _  _ k (Set ss) =
    [ P ("sets" <> k <> "count") (fromIntegral $ S.size ss) ]
calculate qs _ k (Timer vs) = concatMap (quantile k xs) qs <>
    [ P ("timers" <> k <> "std")   $ stdDev xs
    , P ("timers" <> k <> "upper") $ V.last xs
    , P ("timers" <> k <> "lower") $ V.head xs
    , P ("timers" <> k <> "count") . fromIntegral $ V.length xs
    , P ("timers" <> k <> "sum")   $ V.sum xs
    , P ("timers" <> k <> "mean")  $ mean xs
    ]
  where
    xs = sort vs

quantile :: Key -> V.Vector Double -> Int -> [Point]
quantile k xs q =
    [ P ("timers" <> k <> a "mean_")  $ mean ys
    , P ("timers" <> k <> a "upper_") $ V.last ys
    , P ("timers" <> k <> a "sum_")   $ V.sum ys
    ]
  where
    a  = Key . (`BS.append` BS.pack (show q))
    n  = round $ fromIntegral q / 100 * (fromIntegral $ V.length xs :: Double)
    ys = V.take n xs
