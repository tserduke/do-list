-- | Construct lists using do notation.
-- Example usage <https://github.com/tserduke/do-list#examples>.
--
-- Control.Monad.Writer from the mtl package provides a somewhat faster yet less flexible Monoid-based alternative.
module Data.DoList
  ( DoList
  , DoListM (DoList)
  -- * Construction
  , item
  -- * List conversion
  , fromList
  , toList
  ) where

import Data.DoMonoid

import GHC.Exts (IsList, IsString, Item, fromString)
import qualified GHC.Exts as E (fromList, toList)


type DoList a = DoListM a ()

-- | 'DoList' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them with purely phantom result type.
newtype DoListM a r = DoList (DoMonoidM [a] r)
  deriving (Eq, Ord, Read, Show, Functor, Applicative, Monad)

instance (IsString a) => IsString (DoListM a r) where
  {-# INLINE fromString #-}
  fromString = item . fromString

instance IsList (DoListM a r) where
  type Item (DoListM a r) = a
  {-# INLINE fromList #-}
  fromList = fromList
  {-# INLINE toList #-}
  toList = toList

-- | Create a 'DoList' holding a single item.
{-# INLINE item #-}
item :: a -> DoListM a r
item = fromList . pure


-- | Convert from a list.
{-# INLINE fromList #-}
fromList :: [a] -> DoListM a r
fromList = DoList . DoM

-- | Convert to a list.
{-# INLINE toList #-}
toList :: DoListM a r -> [a]
toList (DoList x) = runDoM x
