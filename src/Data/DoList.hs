-- | Construct lists using do notation syntatic sugar. Example usage <https://github.com/tserduke/do-list>.

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

-- | 'DoList' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them to enable syntatic sugar of do notation.
-- Its result type is purely phantom.
newtype DoList a r = DoList (D.DList a)
  deriving (Eq, Ord, Read, Show)

-- | Extract the underlying 'D.DList'.
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


-- | Create a 'DoList' holding a single item.
{-# INLINE item #-}
item :: a -> DoList a r
item = DoList . D.singleton

-- | Convert from list.
{-# INLINE fromList #-}
fromList :: [a] -> DoList a r
fromList = DoList . D.fromList

-- | Convert to list.
{-# INLINE toList #-}
toList :: DoList a r -> [a]
toList = D.toList . unDoList


notSupported :: String -> a
notSupported func = error $ "DoList " ++ func ++ " is not supported!"
