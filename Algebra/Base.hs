module Algebra.Base where

-- * Import list
import Algebra.Suspension
import Algebra.Symbol

import Data.List ( genericReplicate )
import Data.Map ( Map )
import qualified Data.Map as Map

import Util.Tuple
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

-- * Data declarations, instances and type synonyms 
type Coefficient = Integer
type Exponent = Integer

-- * SymbolicConstant represents a known value, that can appear in an equation, but not be resolved into a number
data SymbolicConstant = Symbol :. (Maybe Integer) 

instance Eq SymbolicConstant where
  (s :. n) == (s' :. n') | symbol s == symbol s' = n == n'
                         | otherwise = False

instance Ord SymbolicConstant where
  (s :. n) `compare` (s' :. n') = case symbol s `compare` symbol s' of
    EQ -> n `compare` n'
    neq -> neq

data Variable = SC SymbolicConstant | R Recurrence deriving (Eq, Ord)

varR :: Variable -> Bool
varR (R _) = True
varR _ = False
varSC :: Variable -> Bool
varSC (SC _) = True
varSC _ = False

-- * Here I define Recurrences and this is the real meat of the program. It is also far from perfect, and will almost certainly be changed often in the near future.

type RecurrenceSuspension = Suspension Integer Polynomial

-- | Recurrence
data Recurrence = Recurrence Latin.Letter RecurrenceSuspension

-- f `recurOn` g = Suspension (f . g)

expandRecurrence :: Recurrence -> Polynomial
expandRecurrence (Recurrence _ rs) = release rs

expandPolynomial :: Polynomial -> Polynomial
expandPolynomial p = foldr (.+.) zero $ map expandMonomial $ splitPolynomial p

expandMonomial :: Monomial -> Polynomial
expandMonomial ((T m) :* c) = distributeMOverP constantMonomial superPolynomial
  where constantMonomial = T constantSubterm :* c
        superPolynomial = foldr (.*.) one $ zipWith (.^) recurrencePolynomials recurrenceExps
        recurrencePolynomials = map (release . (\(R (Recurrence _ r)) -> r)) recurrenceVars
        (recurrenceVars, recurrenceExps) = unzip $ map tv2ve $ splitTerm (T recurrenceSubterm)
        (recurrenceSubterm, constantSubterm) = Map.partitionWithKey (\k _ -> varR k) m

instance Eq Recurrence where
  (Recurrence l (Suspension _ n)) == (Recurrence l' (Suspension _ n')) | l == l' = n == n'
                                                                           | otherwise = False
instance Ord Recurrence where
  (Recurrence l (Suspension _ n)) `compare` (Recurrence l' (Suspension _ n')) = case l `compare` l' of
    EQ -> n `compare` n'
    neq -> neq

-- | Term is without coefficient, Monomial is with coefficient
data Monomial = Term :* Coefficient 
type M = Monomial

data Term = T (Map Variable Exponent) deriving (Eq, Ord)

data TermVar = Variable :^ Exponent

-- | A polynomial is a a map from Terms to Coefficients, and not from Monomials
data Polynomial = P (Map Term Coefficient) 
type P = Polynomial
type Function = Integer -> P

(.+.) :: P -> P -> P
(.+.) (P m) (P m') = P $ Map.unionWith (+) m m'

(.*.) :: P -> P -> P
(.*.) p@(P m) p'@(P m') = foldr (.+.) zero polynomials
  where polynomials = (map $ flip distributeMOverP p') monomials
        monomials = splitPolynomial p

(.^) :: P -> Integer -> P
(.^) p n = foldr (.*.) one $ genericReplicate n p

infixr .+.

distributeMOverP :: Monomial -> Polynomial -> Polynomial
distributeMOverP m = mergeMonomials . map (multiplyMs m) . splitPolynomial

multiplyMs :: Monomial -> Monomial -> Monomial
multiplyMs ((T xs) :* c) ((T ys) :* d) = T (Map.unionWith (+) xs ys) :* (c * d)

data RecurrencePerspective = RecurrencePerspective { recurrence :: Recurrence,
                                                     printedConstants :: [SymbolicConstant],
                                                     printExponentWhen1 :: Bool,
                                                     printCoefficientWhen1 :: Bool
                                                   }
-- | Monomial to (Term, Coefficient)
m2tc = \(t :* c) -> (t, c)
-- | (Term, Coefficient) to Monomial
tc2m = \(t, c) -> (t :* c)

-- | TermVar to (Variable, Exponent)
tv2ve = \(v :^ e) -> (v, e)
-- | (Variable, Exponent) to TermVar
ve2tv = \(v, e) -> (v :^ e)

constantTerm :: Integer -> Monomial
constantTerm n = T Map.empty :* n

zero :: Polynomial
zero = P Map.empty

one = P $ Map.singleton (T Map.empty) 1

emptyConstantTerms :: [SymbolicConstant] -> [Monomial]
emptyConstantTerms = map (\s -> T (only1 $ SC s) :* 0)

splitTerm :: Term -> [TermVar] 
splitTerm = map ve2tv . Map.assocs . (\(T m) -> m)

-- | Note that this takes care of adding degrees of variables together, so that I don't have to do that myself
mergeTermVars :: [TermVar] -> Term
mergeTermVars = T . Map.fromListWith (+) . map tv2ve

splitPolynomial :: Polynomial -> [Monomial]
splitPolynomial = map tc2m . Map.assocs . (\(P m) -> m)

-- | Note that this takes care of adding coefficients of terms together so I don't have to do that myself
mergeMonomials :: [Monomial] -> Polynomial
mergeMonomials = P . Map.fromListWith (+) . map m2tc

only1 = \m -> Map.singleton m 1
