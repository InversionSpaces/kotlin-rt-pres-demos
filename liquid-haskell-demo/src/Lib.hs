{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module Lib (demo) where

-- (1) pre and post-conditions for functions
-- predicates are from a sublanguage!

{-@ increment :: {v:Int | v >= 0} -> {x:Int | x > 0} @-}
increment :: Int -> Int
increment x = x + 1

{-@ safeDivide :: Int -> {v:Int | v /= 0} -> Int @-}
safeDivide :: Int -> Int -> Int
safeDivide x y = x `div` y

-- (2) refinement types

{-@ type Pos = {v:Int | v > 0} @-}

{-@ posMul :: Pos -> Pos -> Pos @-}
posMul :: Int -> Int -> Int
posMul x y = x * y

-- (3) parametrized refinements

-- by values
{-@ type BoundedInt Lo Hi = {v:Int | Lo <= v && v <= Hi} @-}

-- by types and values
{-@ type ListN a N = {v:[a] | len v == N} @-}

{-@ percentage :: BoundedInt 0 100 -> Double @-}
percentage :: Int -> Double
percentage x = fromIntegral x / 100.0

-- (4) dependent refinements

{-@ takeSafe :: n:Pos -> xs:{l: [a] | len l >= n} -> ListN a n @-}
takeSafe :: Int -> [a] -> [a]
takeSafe = take

-- helper function for demonstration
demo :: IO ()
demo = do
  putStrLn "Pre/Post Conditions"
  print (increment 10)
  print (safeDivide 10 2)
  print (posMul 3 4)

  putStrLn "\nParametrized Refinements"
  print (percentage 85)

  putStrLn "\nDependent Refinements"
  print (takeSafe 2 [1,2,3])
