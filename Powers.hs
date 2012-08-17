module Powers where

import Util.Display
import qualified Util.Unicode as U
import qualified Util.Unicode.Greek as Greek

data Two = TwoTimes Two Bool | JustTwo

toTwos :: Integer -> Two
toTwos 1 = JustTwo 
toTwos n = let (m, r) = n `quotRem` 2
           in TwoTimes (toTwos m) (r == 1)
              
instance Display Two where
  display JustTwo = "2"
  display (TwoTimes JustTwo b) = "2 * 2 + " ++ if b then "1" else "0"
  display (TwoTimes t b) = "2 * (" ++ display t ++ ") + " ++ if b then "1" else "0"
  
betas :: Two -> (Int, Int)
betas JustTwo = (1, 0)
betas (TwoTimes t b) = update $ betas t
  where update (x,y) = (\(a,b) -> (3*a, 3*b)) (x + if not b then 1 else 0, 3 * y + if b then 1 else 0)