module Algebra.Display where

import Algebra.Base
import Algebra.Symbol

import Data.List ( intersperse )
import qualified Data.Map as Map

import Util.Display
import Util.Numeric
import Util.String
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

instance Symbolic Greek.Letter where
  symbol = U.c2s . Greek.lowercaseLetter

instance Symbolic Latin.Letter where
  symbol = U.c2s . Latin.lowercaseLetter

instance Display Polynomial where 
  display (P m) = concat $ intersperse " + " $ map display $ toTerms m
  
instance Display Term where  
  display (v :* c) = coefficientString c ++ display v 

showPowerRemainder :: Integer -> Integer -> String
x `showPowerRemainder` b = let (m, r) = x `powerRemainder` b in show b ++ superscriptNumber m ++ " + " ++ show r 

superscriptDigit :: Integer -> String
superscriptDigit 1 = ""
superscriptDigit d = U.c2s $ U.superscriptDigit d

coefficientString :: Integer -> String
coefficientString 1 = ""
coefficientString c = display $ c -- `inBase` 4

instance Display Monomial where 
  display (M m) = concat $ map (\(l, p) -> display l ++ (superscriptDigit p)) (Map.assocs m)

instance Display SymbolicConstant where
  display (l :. Nothing) = display l
  display (l :. (Just n)) = display l ++ (subscriptNumber n)

instance Display Variable where
  display (SC c) = display c
  display (R r) = display r

instance Display Recurrence where
  display (Recurrence f _ n) = (U.c2s $ Latin.lowercaseLetter f) ++ (parentheses $ display n)

