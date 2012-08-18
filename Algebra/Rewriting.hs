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
g m | m == 1 = placeholders .+. (p [M alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = p [M gamma :* n, M (beta_ r) :* 1]
        back = (distribute (M Map.empty :* 3) (g n))
        (n, r) = m `divMod` 2        
        placeholders = p $ emptyConstantTerms [Sym Greek.Alpha :. Nothing, 
                                               Sym Greek.Beta :. (Just 0), 
                                               Sym Greek.Beta :. (Just 1), 
                                               Sym Greek.Gamma :. Nothing
                                              ]

fib :: Function
fib 1 = p $ [constantTerm 1]
fib 2 = p $ [constantTerm 1]
fib n = fib' .+. fib''
  where fib' = p [(M $ only1 $ R $ Recurrence Latin.F fib (n - 1)) :* 1]
        fib'' = p [(M $ only1 $ R $ Recurrence Latin.F fib (n - 2)) :* 1]
        
h :: Function
h m | m == 1 = placeholders .+. (p [M alpha :* 1])
    | otherwise = placeholders .+. front .+. back
  where front = p [M (gamma_ r) :* n, M (beta_ r) :* 1]
        back = (distribute (constantTerm 4) (h n))
        (n, r) = m `divMod` 2
        placeholders = p $ emptyConstantTerms [Sym Greek.Alpha :. Nothing,
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

alpha = only1 $ SC $ Sym Greek.Alpha :. Nothing
alpha_ n = only1 $ SC $ Sym Greek.Alpha :. (Just n)
beta = only1 $ SC $ Sym Greek.Beta :. Nothing
beta_ n = only1 $ SC $ Sym Greek.Beta :. (Just n)
gamma = only1 $ SC $ Sym Greek.Gamma :. Nothing
gamma_ n = only1 $ SC $ Sym Greek.Gamma :. (Just n)
  
printTable :: Function -> Integer -> IO ()
printTable f n = do 
  let results = map (display . f) [1..n]
      formattedResults = alignAtChar '+' results
      formattedNs = alignAtChar '+' $ map show {--(`showPowerRemainder` 2)--} [1..n]
      table = zipWith (\a b -> a ++ " | " ++ b) formattedNs formattedResults
  putStrLn $ unlines table
