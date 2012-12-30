{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP                  #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE DefaultSignatures    #-}
#endif

module Generics.Deriving.GBuild
    ( -- * generic builder class
      GBuild(..)

      -- * default definition
    , gbuildbPrecdefault

      -- * utility functions
    , buildString
    , buildChar
    , buildParen
    , buildBS
    , buildFFloat
    , (<&>)
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.List
import Data.Monoid
import Data.Foldable                  (Foldable, toList)
import GHC.Generics

import qualified Data.ByteString as BS


appPrec :: Int
appPrec = 2

data Type = Rec | Tup | Pref | Inf String

type BuildB = Builder -> Builder

class GBuild' f where
    gbuildbPrec' :: Type -> Int -> f a -> BuildB
    isNullary    :: f a -> Bool
    isNullary = error "generic build (isNullary): unnecessary case"

instance GBuild' U1 where
    gbuildbPrec' _ _ U1 = id
    isNullary _ = True

instance (GBuild c) => GBuild' (K1 i c) where
    gbuildbPrec' _ n (K1 a) = gbuildbPrec n a
    isNullary _ = False

instance (GBuild' a, Constructor c) => GBuild' (M1 C c a) where
    gbuildbPrec' _ n c@(M1 x) =
      case fixity of
        Prefix    -> buildParen (n > appPrec && not (isNullary x))
                      ( buildString (conName c)
                      . if isNullary x then id else buildChar ' '
                      . buildBraces t (gbuildbPrec' t appPrec x))
        Infix _ m -> buildParen (n > m) (buildBraces t (gbuildbPrec' t m x))
        where
            fixity = conFixity c

            t | conIsRecord c = Rec
              | conIsTuple  c = Tup
              | otherwise     = case fixity of
                                    Prefix    -> Pref
                                    Infix _ _ -> Inf (show (conName c))

            buildBraces :: Type -> BuildB -> BuildB
            buildBraces Rec     p = buildChar '{' . p . buildChar '}'
            buildBraces Tup     p = buildChar '(' . p . buildChar ')'
            buildBraces Pref    p = p
            buildBraces (Inf _) p = p
            conIsTuple y = tupleName (conName y) where
                tupleName ('(':',':_) = True
                tupleName _           = False

instance (Selector s, GBuild' a) => GBuild' (M1 S s a) where
    gbuildbPrec' t n s@(M1 x) | selName s == "" = --buildParen (n > appPrec)
                                                    gbuildbPrec' t n x
                              | otherwise       =   buildString (selName s)
                                                  . buildString " = "
                                                  . gbuildbPrec' t 0 x
    isNullary (M1 x) = isNullary x


instance (GBuild' a) => GBuild' (M1 D d a) where
    gbuildbPrec' t n (M1 x) = gbuildbPrec' t n x

instance (GBuild' a, GBuild' b) => GBuild' (a :+: b) where
    gbuildbPrec' t n (L1 x) = gbuildbPrec' t n x
    gbuildbPrec' t n (R1 x) = gbuildbPrec' t n x

instance (GBuild' a, GBuild' b) => GBuild' (a :*: b) where
    gbuildbPrec' t@Rec     n (a :*: b) =
        gbuildbPrec' t n     a . buildString ", " . gbuildbPrec' t n     b
    gbuildbPrec' t@(Inf s) n (a :*: b) =
        gbuildbPrec' t n     a . buildString s    . gbuildbPrec' t n     b
    gbuildbPrec' t@Tup     n (a :*: b) =
        gbuildbPrec' t n     a . buildChar ','    . gbuildbPrec' t n     b
    gbuildbPrec' t@Pref    n (a :*: b) =
        gbuildbPrec' t (n+1) a . buildChar ' '    . gbuildbPrec' t (n+1) b

    isNullary _ = False


class GBuild a where
    gbuildbPrec :: Int -> a -> BuildB
    gbuildb     :: a -> BuildB
    gbuildb = gbuildbPrec 0
    gbuild      :: a -> Builder
    gbuild x = gbuildb x $ fromByteString BS.empty

#if __GLASGOW_HASKELL__ >= 701

    default gbuildbPrec :: (Generic a, GBuild' (Rep a)) => Int -> a -> BuildB
    gbuildbPrec = gbuildbPrecdefault

instance (GBuild a) => GBuild (Maybe a)

#else

instance (GBuild a) => GBuild (Maybe a) where
    gbuildbPrec = gbuildbPrecdefault

#endif

gbuildbPrecdefault :: (Generic a, GBuild' (Rep a)) => Int -> a -> BuildB
gbuildbPrecdefault n = gbuildbPrec' Pref n . from


instance GBuild Builder       where gbuildbPrec _ x = (x <>)
instance GBuild BS.ByteString where gbuildbPrec _   = buildBS

instance GBuild Bool    where gbuildbPrec = buildShowsPrec
instance GBuild Double  where gbuildbPrec = buildShowsPrec
instance GBuild Float   where gbuildbPrec = buildShowsPrec
instance GBuild Int     where gbuildbPrec = buildShowsPrec
instance GBuild Integer where gbuildbPrec = buildShowsPrec

instance GBuild Char   where gbuildbPrec _ = buildChar
instance GBuild String where gbuildbPrec _ = buildString

instance (GBuild a) => GBuild [a] where
    gbuildbPrec _ l =  buildChar '['
                     . foldr (.) id
                        (intersperse (buildChar ',') (map (gbuildbPrec 0) l))
                     . buildChar ']'

instance (Foldable f, GBuild a) => GBuild (f a) where
    gbuildbPrec n xs = gbuildbPrec n $ toList xs

-- utilities

buildChar :: Char -> BuildB
buildChar c = (fromChar c <>)

buildString :: String -> BuildB
buildString s = (fromString s <>)

buildParen :: Bool -> BuildB -> BuildB
buildParen b p = if b then buildChar '(' . p . buildChar ')' else p

buildBS :: BS.ByteString -> BuildB
buildBS bs = (fromByteString bs <>)

buildShowsPrec :: Show a => Int -> a -> BuildB
buildShowsPrec n c = buildString . showsPrec n c $ ""

-- FIXME: defaulting warning
buildFFloat :: (GBuild a, RealFloat a) => Maybe Int -> a -> BuildB
buildFFloat Nothing  a = (gbuild a <>)
buildFFloat (Just x) a = (gbuild (round' a) <>)
  where
    round' y = (realToFrac . round $ y * exp') / exp'
    exp'     = 10^x
{-# SPECIALIZE buildFFloat ::
        Maybe Int -> Float  -> BuildB,
        Maybe Int -> Double -> BuildB #-}

(<&>) :: (GBuild a, GBuild b) => a -> b -> Builder
a <&> b = gbuild a <> gbuild b
