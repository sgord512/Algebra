{-# LANGUAGE MutilParamTypeClasses #-}
 module Algebra.Suspension where

data Suspension a b = Suspension (a -> b) a