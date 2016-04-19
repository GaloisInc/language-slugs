{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.Lens where

import Data.Functor.Identity (Identity(..))


-- Lenses ----------------------------------------------------------------------

newtype Const r a = Const { unConst :: r
                          } deriving (Functor)

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a

type Getting r s a = (a -> Const r a) -> (s -> Const r s)

view :: Getting a s a -> s -> a
view l = \ s -> unConst (l Const s)
{-# INLINE view #-}


type ASetter s t a b = (a -> Identity b) -> (s -> Identity t)

type ASetter' s a = ASetter s s a a

set :: ASetter s t a b -> b -> s -> t
set l = \ b s -> runIdentity (l (\ _ -> Identity b) s)
{-# INLINE set #-}

over :: ASetter s t a b -> (a -> b) -> (s -> t)
over l = \ f s -> runIdentity (l (\a -> Identity (f a)) s)
{-# INLINE over #-}


type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

rewriteOf :: ASetter' a a -> (a -> Maybe a) -> a -> a
rewriteOf l f = go
  where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

transformOf :: ASetter' a a -> (a -> a) -> a -> a
transformOf l f = go
  where
  go = f . over l go
{-# INLINE transformOf #-}

