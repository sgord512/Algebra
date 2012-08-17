module Algebra.Rewriting where

import Algebra.Base
import Algebra.Display
import Algebra.Symbol

import Data.List ( intersperse )
import Data.Map ( Map )
import qualified Data.Map as Map
import Util.Display
import Util.Numeric
import Util.String.Align
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek
import qualified Util.Unicode.Latin as Latin

g :: Function 
g m | m == 1 = placeholders .+. (P $ fromTerms [M alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = P (fromTerms [M gamma :* n, M (beta_ r) :* 1])
        back = (distribute (M Map.empty :* 3) (g n))
        (n, r) = m `divMod` 2        
        placeholders = P $ fromTerms $ emptyTerms [Sym Greek.Alpha :. Nothing, 
                                                   Sym Greek.Beta :. (Just 0), 
                                                   Sym Greek.Beta :. (Just 1), 
                                                   Sym Greek.Gamma :. Nothing
                                                  ]

fib :: Function
fib 1 = constantTerm 1
fib 2 = constantTerm 2
fib n = (fib $ n - 1) .+. (fib $ n - 2)
        
h :: Function
h m | m == 1 = placeholders .+. (p [M alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = p [M (gamma_ r) :* n, M (beta_ r) :* 1]
        back = (distribute (constantTerm 4) (h n))
        (n, r) = m `divMod` 2
        placeholders = p $ emptyTerms [Sym Greek.Alpha :. Nothing,
                                       Sym Greek.Beta :. (Just 0),
                                       Sym Greek.Beta :. (Just 1),
                                       Sym Greek.Gamma :. (Just 0),
                                       Sym Greek.Gamma :. (Just 1)
                                      ]
               
        
(.+.) :: P -> P -> P
(.+.) (P m) (P m') = P $ Map.unionWith (+) m m'

infixr .+.

distribute :: Term -> P -> P 
distribute t (P terms) = P $ fromTerms $ map (multiply t) (toTerms terms)

multiply :: Term -> Term -> Term
multiply ((M xs) :* c) ((M ys) :* d) = M (Map.unionWith (+) xs ys) :* (c * d)

only1 = \m -> Map.singleton m 1

alpha = only1 $ Sym Greek.Alpha :. Nothing
alpha_ n = only1 $ Sym Greek.Alpha :. (Just n)
beta = only1 $ Sym Greek.Beta :. Nothing
beta_ n = only1 $ Sym Greek.Beta :. (Just n)
gamma = only1 $ Sym Greek.Gamma :. Nothing
gamma_ n = only1 $ Sym Greek.Gamma :. (Just n)

toTerms :: Map M Coefficient -> [Term]
toTerms m = map (\(v, c) -> (v :* c)) . Map.assocs

fromTerms :: [Term] -> Map M Coefficient
fromTerms = Map.fromListWith (+) . map (\(v :* c) -> (v, c))
  
printTable :: Function -> Integer -> IO ()
printTable f n = do 
  let results = map (display . f) [1..n]
      formattedResults = alignAtChar '+' results
      formattedNs = alignAtChar '+' $ map (`showPowerRemainder` 2) [1..n]
      table = zipWith (\a b -> a ++ " | " ++ b) formattedNs formattedResults
  putStrLn $ unlines table
