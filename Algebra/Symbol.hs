{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
module Algebra.Symbol where

import Util.Display
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

data Symbol = forall s. Symbolic s => Sym s

instance Symbolic Symbol where
  symbol (Sym s) = symbol s

class Symbolic a where
  symbol :: a -> String

instance Display Symbol where
  display = symbol