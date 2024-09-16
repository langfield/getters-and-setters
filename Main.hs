module Main (main, ix, over, view, viewM, viewD, (!?), (!!?), set, set2D, setMany) where

import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Control.Applicative (Const (..))

-- | Two-dimensional coordinates.
data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

-- | Generalized list setter and getter.
ix :: Applicative f => Int -> (a -> f a) -> [a] -> f [a]
ix k f xs | k < 0     = pure xs
          | otherwise = go xs k where
    go [] _     = pure []
    go (a:as) 0 = f a  <&> (:as)
    go (a:as) i = (a:) <$> (go as $! i - 1)

-- | A list setter.
over :: Int -> (a -> a) -> [a] -> [a]
over k f = runIdentity . ix k (Identity . f)

-- | A list getter.
view :: Int -> [a] -> Maybe a
view k = getFirst . getConst . ix k (Const . First . Just)

-- | A list getter that defaults to `mempty`.
viewM :: Monoid a => Int -> [a] -> a
viewM k = getConst . ix k Const

-- | A list getter that takes a default.
viewD :: a -> Int -> [a] -> a
viewD d k = fromMaybe d . view k

-- | Index into a list.
(!?) :: [a] -> Int -> Maybe a
(!?) = flip view

-- | Index into a 2d array.
(!!?) :: [[a]] -> Coord -> Maybe a
xss !!? (C i j) = xss !? i >>= (!? j)

-- | A list setter.
set :: Int -> (a -> a) -> [a] -> [a]
set = over

-- | Write a value to a location in a 2d array.
set2D :: Coord -> a -> [[a]] -> [[a]]
set2D (C i j) = set i . set j . const

-- | Write a value to a list of locations in a 2d array.
setMany :: a -> [[a]] -> [Coord] -> [[a]]
setMany = foldr . flip set2D

main :: IO ()
main = pure ()
