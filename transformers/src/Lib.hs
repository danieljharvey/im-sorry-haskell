{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc
    ) where

newtype Compose f g a = 
    Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure $ pure a

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a 
          -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

-- how to make 
item :: Compose Maybe [] Int
item = Compose $ Just [1,2,3,4,5]

funkyTown :: Compose Maybe Maybe (Int -> Int)
funkyTown = Compose $ Just $ Just (+100)

someFunc :: IO ()
someFunc = print "yes"
