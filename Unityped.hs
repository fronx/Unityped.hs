{-# LANGUAGE FlexibleInstances #-}

module Unityped where

import Prelude hiding ((*), print, show)
import qualified Prelude as P

data D = B Bool
       | N Int
       | S String
       | F  (D -> D)
       | F2 ((D, D) -> D)
       | Object [(String, D)]

print d = putStrLn s
  where S s = show $$ d

show = F f
  where f (B True)   = S "true"
        f (B False)  = S "false"
        f (N n)      = S (P.show n)
        f (S s)      = S s
        f (F _)      = S "(function)"

class Apply argType where
  ($$) :: D -> argType -> D

instance Apply D where
  (F f) $$ d = f d

instance Apply (D, D) where
  (F2 f) $$ (d, d') = f (d, d')

mul = F2 f
  where f (x    , (N 1)) = x
        f ((N n), (N m)) = N (n P.* m)
        f ((S s), (N m)) = mul $$ ((S (s ++ s)), (N (m - 1)))
        f ((F g), (N m)) = mul $$ ((F (g .  g)), (N (m - 1)))

a * b = mul $$ (a, b)

main = do
  print (B True)
  print (B False)
  print (N 123)
  print (S "hello")
  print show
  print $ (N 2)    * (N 3)
  print $ (S "hi") * (N 3)
  -- print $ (B True) * (N 3) -- "type error"
