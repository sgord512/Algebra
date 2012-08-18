module Algebra.Base where

-- * Import list
import Algebra.Symbol

import Data.Map ( Map )
import qualified Data.Map as Map

import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

type Coefficient = Integer
type Exponent = Integer

data SymbolicConstant = Symbol :. (Maybe Integer) 

instance Eq SymbolicConstant where
  (s :. n) == (s' :. n') | symbol s == symbol s' = n == n'
                         | otherwise = False

instance Ord SymbolicConstant where
  (s :. n) `compare` (s' :. n') = case symbol s `compare` symbol s' of
    EQ -> n `compare` n'
    neq -> neq

data Variable = SC SymbolicConstant | R Recurrence deriving (Eq, Ord)

-- | Recurrence here actually means a RecurrenceApplication, but that is unwieldy, so I am ignoring that for the moment.
data Recurrence = Recurrence Latin.Letter Function Integer

instance Eq Recurrence where
  (Recurrence l _ n) == (Recurrence l' _ n') | l == l' = n == n'
                                             | otherwise = False
instance Ord Recurrence where
  (Recurrence l _ n) `compare` (Recurrence l' _ n') = case l `compare` l' of
    EQ -> n `compare` n'
    neq -> neq

type Function = Integer -> P 

data Monomial = M (Map Variable Exponent) deriving (Eq, Ord)
type M = Monomial

data Polynomial = P (Map Monomial Coefficient) 
type P = Polynomial

-- | Term is with coefficient, Monomial is without coefficient
data Term = Monomial :* Coefficient 

data RecurrencePerspective = RecurrencePerspective { recurrence :: Recurrence,
                                                     printedConstants :: [SymbolicConstant],
                                                     printExponentWhen1 :: Bool,
                                                     printCoefficientWhen1 :: Bool
                                                   }
                                                     

p = P . fromTerms
polynomial = P . fromTerms

constantTerm n = M Map.empty :* n

emptyConstantTerms :: [SymbolicConstant] -> [Term]
emptyConstantTerms = map (makeZeroTerm . SC)
  where makeZeroTerm letter = M (firstDegreeTerm letter) :* 0
        firstDegreeTerm letter = only1 letter

toTerms :: Map M Coefficient -> [Term]
toTerms = map (\(v, c) -> (v :* c)) . Map.assocs

fromTerms :: [Term] -> Map M Coefficient
fromTerms = Map.fromListWith (+) . map (\(v :* c) -> (v, c))

only1 = \m -> Map.singleton m 1
