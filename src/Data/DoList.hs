module Data.DoList
  ( DoList (DoList)
  , item
  , fromList
  , toList
  ) where

import qualified Data.DList as D


newtype DoList a r = DoList (D.DList a)
  deriving (Eq, Ord, Read, Show)


instance Functor (DoList a) where
  fmap = notSupported "fmap"

instance Applicative (DoList a) where
  pure = notSupported "pure"
  (<*>) = notSupported "(<*>)"

instance Monad (DoList a) where
  (>>=) = notSupported "(>>=)"
  {-# INLINE (>>) #-}
  (DoList xs) >> (DoList ys) = DoList $ D.append xs ys


{-# INLINE fromList #-}
fromList :: [a] -> DoList a r
fromList = DoList . D.fromList

{-# INLINE toList #-}
toList :: DoList a r -> [a]
toList (DoList xs) = D.toList xs

{-# INLINE item #-}
item :: a -> DoList a r
item = DoList . D.singleton


notSupported :: String -> a
notSupported func = error $ "DoList " ++ func ++ " is not supported!"
