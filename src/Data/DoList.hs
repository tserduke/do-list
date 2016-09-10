{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

-- | Construct lists using do notation.
-- Example usage <https://github.com/tserduke/do-list#examples>.
--
-- Control.Monad.Writer from the mtl package provides a somewhat faster yet less flexible Monoid-based alternative.
module Data.DoList
  ( DoList (DoList)
  -- * Construction
  , item
  -- * List conversion
  , fromList
  , toList
  ) where

import Data.DoMonoid

import GHC.Exts (IsList, IsString, Item, fromString)
import qualified GHC.Exts as E (fromList, toList)


-- | 'DoList' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them with purely phantom result type.
newtype DoList a r = DoList (DoMonoid [a] r)
  deriving (Eq, Ord, Read, Show, Functor, Applicative, Monad)

instance IsList (DoList a r) where
  type Item (DoList a r) = a
  fromList = fromList
  toList = toList

instance (IsString a) => IsString (DoList a r) where
  {-# INLINE fromString #-}
  fromString = item . fromString


-- | Create a 'DoList' holding a single item.
{-# INLINE item #-}
item :: a -> DoList a r
item = fromList . pure


-- | Convert from a list.
{-# INLINE fromList #-}
fromList :: [a] -> DoList a r
fromList = DoList . DoM

-- | Convert to a list.
{-# INLINE toList #-}
toList :: DoList a r -> [a]
toList (DoList x) = runDoM x
