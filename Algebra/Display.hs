{-# LANGUAGE RecordWildCards #-}
module Algebra.Display where

import Algebra.Base
import Algebra.Suspension
import Algebra.Symbol

import Data.List ( intersperse )
import Data.Map ( Map )
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
  display p = concat $ intersperse " + " $ map display $ splitPolynomial p
  
instance Display Monomial where  
  display (v :* c) = display c ++ display v 

displayMonomial :: Bool -> Bool -> Monomial -> String
displayMonomial showC showE (v :* c) = displayCoefficient c ++ (displayTerm showE v)
  where displayCoefficient = (if showC then hideOnes else id) display

hideOnes :: (Integer -> String) -> Integer -> String
hideOnes f 1 = ""
hideOnes f n = f n 

showPowerRemainder :: Integer -> Integer -> String
x `showPowerRemainder` b = let (m, r) = x `powerRemainder` b in show b ++ superscriptNumber m ++ " + " ++ show r 

instance Display Term where 
  display (T m) = concat $ map (\(l, p) -> display l ++ (superscriptNumber p)) (Map.assocs m)

displayTerm :: Bool -> Term -> String
displayTerm showE (T m) = concat $ map (\(l, p) -> display l ++ (displayExponent p)) (Map.assocs m)
  where displayExponent = (if showE then hideOnes else id) superscriptNumber

instance Display SymbolicConstant where
  display (l :. Nothing) = display l
  display (l :. (Just n)) = display l ++ (subscriptNumber n)

instance Display Variable where
  display (SC c) = display c
  display (R r) = display r

instance Display Recurrence where
  display (Recurrence f (Suspension _ n)) = (U.c2s $ Latin.lowercaseLetter f) ++ (parentheses $ display n)

instance Display RecurrencePerspective where
  display RecurrencePerspective{..} = display recurrence ++ " = " ++ displayedPolynomial
    where displayedPolynomial = concat $ intersperse " + " $ map (displayMonomial printCoefficientWhen1 printExponentWhen1) $ splitPolynomial polynomial
          polynomial = expandRecurrence recurrence .+. emptyPolynomial
          emptyPolynomial = mergeMonomials $ emptyConstantTerms printedConstants