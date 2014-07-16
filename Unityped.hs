module Unityped where

import Prelude hiding ((*), print, show)
import qualified Prelude as P

data D = B Bool
       | N Int
       | S String
       | F (D -> D)
       | D1 D
       | D2 D D
       | D3 D D D
       | Object [(String, D)]

print d = putStrLn s
  where S s = show $$ d

show = F f
  where f (B True)   = S "true"
        f (B False)  = S "false"
        f (N n)      = S (P.show n)
        f (S s)      = S s
        f (F _)      = S "(function)"
        f (Object _) = S "(object)"

-- apply dynamic functions
(F f) $$ d = f d

mul = F f
  where f (D2 x     (N 1)) = x
        f (D2 (N n) (N m)) = N (n P.* m)
        f (D2 (S s) (N m)) = mul $$ (D2 (S (s ++ s)) (N (m - 1)))
        f (D2 (F g) (N m)) = mul $$ (D2 (F (g .  g)) (N (m - 1)))

a * b = mul $$ (D2 a b)

alan = Object [
  ("name",         S "Alan Kay"),
  ("yearOfBirth",  N 1940) ]

main = do
  print (B True)
  print (B False)
  print (N 123)
  print (S "hello")
  print show
  print $ (N 2)    * (N 3)
  print $ (S "hi") * (N 3)
  --print $ (B True) * (N 3)
  --print alan
