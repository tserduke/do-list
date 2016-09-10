{-# LANGUAGE TypeFamilies #-}

-- | Construct lists using do notation.
-- Example usage <https://github.com/tserduke/do-list#examples>.
--
-- Control.Monad.Writer from the mtl package provides a somewhat faster yet less flexible Monoid-based alternative.
module Data.DoList
  ( DoList (DoList)
  -- * Construction
  , item
  , toDList
  -- * List conversion
  , fromList
  , toList
  ) where

import qualified Data.DList as D
import GHC.Exts (IsList, IsString, Item, fromString)
import qualified GHC.Exts as E (fromList, toList)


-- | 'DoList' is not a real instance of 'Monad', 'Applicative' or 'Functor'.
-- It pretends being them with purely phantom result type.
newtype DoList a r = DoList (D.DList a)
  deriving (Eq, Ord, Read, Show)

-- | Extract the underlying 'D.DList'.
{-# INLINE toDList #-}
toDList :: DoList a r -> D.DList a
toDList (DoList x) = x


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
  (>>) (DoList x) = DoList . D.append x . toDList

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
item = DoList . D.singleton


-- | Convert from a list.
{-# INLINE fromList #-}
fromList :: [a] -> DoList a r
fromList = DoList . D.fromList

-- | Convert to a list.
{-# INLINE toList #-}
toList :: DoList a r -> [a]
toList = D.toList . toDList


notSupported :: String -> a
notSupported func = error $ "DoList " ++ func ++ " is not supported!"
