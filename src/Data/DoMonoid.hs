module Data.DoMonoid
  ( DoMonoid (DoM)
  , runDoM
  ) where

import GHC.Exts (IsList, IsString, Item, fromList, toList)


newtype DoMonoid m r = DoM m
  deriving (Eq, Ord, Read, Show, IsString)

-- | Unwrap the monoid.
{-# INLINE runDoM #-}
runDoM :: DoMonoid m r -> m
runDoM (DoM x) = x


-- | Functor operations are not supported.
instance Functor (DoMonoid a) where
  fmap = notSupported "fmap"

-- | Applicative operations are not supported.
instance Applicative (DoMonoid a) where
  pure = notSupported "pure"
  (<*>) = notSupported "(<*>)"

-- | Monadic operations are not supported.
instance (Monoid a) => Monad (DoMonoid a) where
  (>>=) = notSupported "(>>=)"
  {-# INLINE (>>) #-}
  (DoM x) >> (DoM y) = DoM $ x `mappend` y

instance (IsList m) => IsList (DoMonoid m r) where
  type Item (DoMonoid m r) = Item m
  {-# INLINE fromList #-}
  fromList = DoM . fromList
  {-# INLINE toList #-}
  toList = toList . runDoM


notSupported :: String -> a
notSupported func = error $ "DoMonoid " ++ func ++ " is not supported!"
