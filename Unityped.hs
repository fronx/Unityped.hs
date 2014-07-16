{-# LANGUAGE FlexibleInstances #-}

module Unityped where

import Prelude hiding ((*), print, show)
import qualified Prelude as P

data D = B Bool
       | N Int
       | S String
       | F (D -> D)
       | F2 ((D, D) -> D)

print d = putStrLn s
  where S s = show $$ d

show = F f
  where f (B True)   = dyn "true"
        f (B False)  = dyn "false"
        f (N n)      = dyn (P.show n)
        f (S s)      = dyn s
        f (F _)      = dyn "(function)"

class Apply argType where
  ($$) :: D -> argType -> D

instance Apply D where
  (F f) $$ d = f d

instance Apply (D, D) where
  (F2 f) $$ (d, d') = f (d, d')

mul = dyn f
  where f (x    , (N 1)) = x
        f ((N n), (N m)) = dyn (n P.* m)
        f ((S s), (N m)) = mul $$ ((dyn (s ++ s)), (dyn (m - 1)))
        f ((F g), (N m)) = mul $$ ((dyn (g .  g)), (dyn (m - 1)))

a * b = mul $$ (a, b)

class Dyn a where
  dyn :: a -> D

instance Dyn Bool     where dyn = B
instance Dyn Int      where dyn = N
instance Dyn String   where dyn = S
instance Dyn (D -> D) where dyn = F
instance Dyn ((D, D) -> D) where dyn = F2

main = do
  print (dyn True)
  print (dyn False)
  print (N 123)
  print (dyn "hello")
  print show
  print $ (N 2)      * (N 3)
  print $ (dyn "hi") * (N 3)
  print $ (dyn "hi") * (N 3) * (N 2) * (dyn "abc") -- "type error"
  -- print $ (B True) * (N 3) -- "type error"
  print $ dyn True
