module Functions
( sinc
, convolve
) where

import Data.List (tails)

----------

{- Boosted from Boost http://www.boost.org/boost/math/special_functions/sinc.hpp -}
sinc :: (RealFloat a) => a -> a
sinc x =
   if abs x >= taylor_n_bound
     then sin x / x
     else 1 - x^2/6 + x^4/120
 where
  taylor_n_bound = sqrt $ sqrt epsilon

epsilon :: RealFloat a => a
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)

----------

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
  in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)

