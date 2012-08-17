module Algebra.Base where

import Algebra.Symbol

import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

type Coefficient = Integer
type Power = Integer

data SymbolicConstant = Symbol :. (Maybe Integer) deriving (Eq, Ord, Read, Show)

data Variable = Either SymbolicConstant Recurrence

data Recurrence = Recurrence Latin.Letter Function Integer

type Function = Integer -> P 

data Monomial = M (Map Variable Power) deriving (Eq, Ord, Show)
type M = Monomial

data Polynomial = P (Map Monomial Coefficient) deriving Show
type P = Polynomial

-- | Term is with coifficient, Monomial is without
data Term = Monomial :* Coefficient deriving (Eq, Ord, Show)

type SymbolicConstants = [SymbolicConstant]

data RecurrencePerspective = RecurrencePerspective Recurrence SymbolicConstants

p = P . fromTerms
polynomial = P . fromTerms

constantTerm n = M Map.empty :* n

emptyTerms :: [SymbolicConstant] -> [Term]
emptyTerms c = map makeZeroTerm c 
  where makeZeroTerm letter = M (firstDegreeTerm letter) :* 0
        firstDegreeTerm letter = only letter 1
