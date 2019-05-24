{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-
This allows us to simplify the constraints we need to specify up front for
KnownNat constraints. For example, the following function

f :: forall n . (KnownNat n, KnownNat (n+2)) => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))

can be simplified to

f :: forall n . KnownNat n => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))
-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- Allows us to equate terms like (a + b) + c and a + (b + c) as the same thing.
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main where

import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Data.Proxy
import           Numeric.LinearAlgebra.MatrixFree

f :: forall n . KnownNat n => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy (n+2))

(.*) :: forall m n. (KnownNat m, KnownNat n) => Double -> L m n -> L m n
(.*) t m = konst t * m

-- Convert continuous time linear dynamical system to discrete time using a
-- given sampling period.
c2d :: forall m n. (KnownNat m, KnownNat n) => Double -> L n n -> L n m -> (L n n, L n m)
c2d t a b = splitCols $ fst (splitRows exp_block)
  where
    exp_block = expm (t .* block)
    block = a ||| b
              ===
               0

main :: IO ()
main = do
  putStrLn "Hello, World!"
  print $ f (Proxy @3)
  print $ new_id "Hello"

  let a = matrix [2, 3, 1, 0] :: L 2 2
  let b = eye :: Sq 2
  let t = 0.1 :: Double
  let (ad, bd) = c2d t a b

  putStrLn "Ad"
  disp 4 ad

  putStrLn "Bd"
  disp 4 bd
