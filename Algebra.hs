module Algebra where

import Data.List ( intersperse )
import Data.Map ( Map )
import qualified Data.Map as Map
import Util.Display
import Util.Numeric
import Util.String.Align
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek

data Vars = Vars (Map Constant Power) deriving (Eq, Ord, Show)

data Term = Vars :* Coefficient deriving (Eq, Ord, Show)

type Coefficient = Integer
type Power = Integer

type Base = Greek.Letter

data Constant = Greek.Letter :. (Maybe Integer) deriving (Eq, Ord, Read, Show)

newtype Sum = Sum (Map Vars Coefficient) deriving Show

g :: Integer -> Sum   
g m | m == 1 = placeholders .+. (Sum $ fromTerms [Vars alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = Sum (fromTerms [Vars gamma :* n, Vars (beta' r) :* 1])
        back = (distribute (Vars Map.empty :* 3) (g n))
        (n, r) = m `divMod` 2        
        placeholders = Sum $ fromTerms $ emptyTerms [Greek.Alpha :. Nothing, 
                                                     Greek.Beta :. (Just 0), 
                                                     Greek.Beta :. (Just 1), 
                                                     Greek.Gamma :. Nothing
                                                    ]
        
h :: Integer -> Sum
h m | m == 1 = placeholders .+. (Sum $ fromTerms [Vars alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = Sum (fromTerms [Vars (gamma' r) :* n, Vars (beta' r) :* 1])
        back = (distribute (Vars Map.empty :* 4) (h n))
        (n, r) = m `divMod` 2
        placeholders = Sum $ fromTerms $ emptyTerms [Greek.Alpha :. Nothing,
                                                     Greek.Beta :. (Just 0),
                                                     Greek.Beta :. (Just 1),
                                                     Greek.Gamma :. (Just 0),
                                                     Greek.Gamma :. (Just 1)
                                                    ]
               
-- placeholders = Sum $ fromTerms emptyTerms

emptyTerms :: [Constant] -> [Term]
emptyTerms c = map makeZeroTerm c 
  where makeZeroTerm letter = Vars (firstDegreeTerm letter) :* 0
        firstDegreeTerm letter = only letter 1
        
(.+.) :: Sum -> Sum -> Sum
(.+.) (Sum m) (Sum m') = Sum $ Map.unionWith (+) m m'

infixr .+.

distribute :: Term -> Sum -> Sum 
distribute t (Sum terms) = Sum $ fromTerms $ map (multiply t) (toTerms terms)

multiply :: Term -> Term -> Term
multiply ((Vars xs) :* c) ((Vars ys) :* d) = Vars (Map.unionWith (+) xs ys) :* (c * d)

only = Map.singleton

alpha = only (Greek.Alpha :. Nothing) 1
alpha' n = only (Greek.Alpha :. (Just n)) 1
beta = only (Greek.Beta :. Nothing) 1
beta' n = only (Greek.Beta :. (Just n)) 1
gamma = only (Greek.Gamma :. Nothing) 1
gamma' n = only (Greek.Gamma :. (Just n)) 1

toTerms :: Map Vars Coefficient -> [Term]
toTerms m = map (\(v, c) -> (v :* c)) (Map.assocs m)

fromTerms :: [Term] -> Map Vars Coefficient
fromTerms ts = Map.fromListWith (+) (map (\(v :* c) -> (v, c)) ts)

instance Display Sum where 
  display (Sum m) = concat $ intersperse " + " $ map display $ toTerms m
  
instance Display Term where  
  display (v :* c) = coefficientString c ++ display v 
  
superscriptDigit :: Integer -> String
superscriptDigit 1 = ""
superscriptDigit d = U.c2s $ U.superscriptDigit d

-- coefficientString 1 = ""
coefficientString c = display $ c -- `inBase` 4

instance Display Vars where 
  display (Vars m) = concat $ map (\(l, p) -> display l ++ (superscriptDigit p)) (Map.assocs m)
  
instance Display Greek.Letter where
  display = U.c2s . Greek.lowercaseLetter

instance Display Constant where
  display (l :. Nothing) = display l
  display (l :. (Just n)) = display l ++ (subscriptNumber n)

showPowerRemainder :: Integer -> Integer -> String
x `showPowerRemainder` b = let (m, r) = x `powerRemainder` b in show b ++ superscriptNumber m ++ " + " ++ show r 
  
printTable :: (Integer -> Sum) -> Integer -> IO ()
printTable f n = do 
  let results = map (display . f) [1..n]
      formattedResults = alignAtChar '+' results
      formattedNs = alignAtChar '+' $ map (`showPowerRemainder` 2) [1..n]
      table = zipWith (\a b -> a ++ " | " ++ b) formattedNs formattedResults
  putStrLn $ unlines table
