{-# LANGUAGE ExistentialQuantification #-}
module Algebra.Symbol where

import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

data Symbol = forall s. Symbolic s => Sym s

class Symbolic a where
  symbol :: a -> String

instance Symbolic Greek.Letter where
  symbol = display

instance Symbolic Latin.Letter where
  symbol = display


