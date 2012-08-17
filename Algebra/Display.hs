module Algebra.Display where

import Algebra.Base
import Algebra.Symbol

import Util.Display
import Util.Numeric
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

instance Display P where 
  display (P m) = concat $ intersperse " + " $ map display $ toTerms m
  
instance Display Term where  
  display (v :* c) = coefficientString c ++ display v 

showPowerRemainder :: Integer -> Integer -> String
x `showPowerRemainder` b = let (m, r) = x `powerRemainder` b in show b ++ superscriptNumber m ++ " + " ++ show r 

superscriptDigit :: Integer -> String
superscriptDigit 1 = ""
superscriptDigit d = U.c2s $ U.superscriptDigit d

-- coefficientString 1 = ""
coefficientString c = display $ c -- `inBase` 4

instance Display M where 
  display (M m) = concat $ map (\(l, p) -> display l ++ (superscriptDigit p)) (Map.assocs m)

instance Display Symbol where
  display (Left g) = display g
  display (Right l) = display l

instance Display Greek.Letter where
  display = U.c2s . Greek.lowercaseLetter

instance Display Latin.Letter where
  display = U.c2s . Latin.lowercaseLetter

instance Display Symbol where
  display (Sym s) = display s

instance Display SymbolicConstant where
  display (l :. Nothing) = display l
  display (l :. (Just n)) = display l ++ (subscriptNumber n)

instance Display Variable where
  display (Left c) = display c
  display (Right r) = display r

instance Display Recurrence where
  display (Recurrence f n) = (U.c2s $ Latin.lowercaseLetter f) ++ (parentheses $ display n)

