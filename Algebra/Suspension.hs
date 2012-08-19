{-# LANGUAGE MultiParamTypeClasses #-}
 module Algebra.Suspension where

data Suspension a b = Suspension (a -> b) a

suspend = Suspension

release :: Suspension a b -> b
release (Suspension f a) = f a