-- | Construct lists with do notation.

module Data.DoList
  ( DoList (DoList)
  -- * Construction
  , item
  , unDoList
  -- * List conversion
  , fromList
  , toList
  ) where

import qualified Data.DList as D


newtype DoList a r = DoList (D.DList a)
  deriving (Eq, Ord, Read, Show)

{-# INLINE unDoList #-}
unDoList :: DoList a r -> D.DList a
unDoList (DoList x) = x

-- | Functor operations are not supported.
instance Functor (DoList a) where
  fmap = notSupported "fmap"

-- | Applicative operations are not supported.
instance Applicative (DoList a) where
  pure = notSupported "pure"
  (<*>) = notSupported "(<*>)"

-- | Monadic operations are not supported.
instance Monad (DoList a) where
  (>>=) = notSupported "(>>=)"
  {-# INLINE (>>) #-}
  (>>) (DoList x) = DoList . D.append x . unDoList


{-# INLINE fromList #-}
fromList :: [a] -> DoList a r
fromList = DoList . D.fromList

{-# INLINE toList #-}
toList :: DoList a r -> [a]
toList = D.toList . unDoList

{-# INLINE item #-}
item :: a -> DoList a r
item = DoList . D.singleton


notSupported :: String -> a
notSupported func = error $ "DoList " ++ func ++ " is not supported!"
