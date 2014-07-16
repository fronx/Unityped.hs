{-# LANGUAGE FlexibleInstances #-}

module Unityped where

import Prelude hiding ((*), (==), print, show, concat)
import qualified Prelude as P

data D = B Bool
       | N Int
       | S String
       | F ([D] -> D)

print d = putStrLn s
  where S s = show $$ [d]

show = dyn f
  where f (B True:[])  = dyn "true"
        f (B False:[]) = dyn "false"
        f (N n:[])     = dyn (P.show n)
        f (S s:[])     = dyn s
        f (F _:[])     = dyn "(function)"

reflect = dyn f
  where f (d:[]) = dyn (_reflect d)

_reflect (B _) = "bool"
_reflect (N _) = "number"
_reflect (S _) = "string"
_reflect (F _) = "function(x)"

($$) :: D -> [D] -> D
(F f) $$ ds = f ds

class DynEq a where
  (==) :: D -> a -> D

instance DynEq Int where
  (N n) == m = dyn (n P.== m)

instance DynEq String where
  (S s) == s' = dyn (s P.== s')

instance DynEq D where
  (B b) == (B b') = dyn (b P.== b')
  (S s) == (S s') = dyn (s P.== s')
  (N n) == (N n') = dyn (n P.== n')

dynf :: (Dyn a, Dyn b) => (a -> b) -> D -> D
dynf f d = dyn (f (nyd d))

dynf2 :: (Dyn a, Dyn b, Dyn c) => (a -> b -> c) -> D -> D -> D
dynf2 f d d' = dyn (f (nyd d) (nyd d'))

decInt = dynf f
  where f :: Int -> Int
        f n = n - 1

mulInt = dynf2 f
  where f :: Int -> Int -> Int
        f = (P.*)

concat = dynf2 f
  where f :: String -> String -> String
        f = (++)

class Dyn a where
  dyn :: a -> D
  nyd :: D -> a

instance Dyn Bool where
  dyn = B
  nyd (B b) = b

instance Dyn Int where
  dyn = N
  nyd (N n) = n

instance Dyn String where
  dyn = S
  nyd (S s) = s

instance Dyn ([D] -> D) where
  dyn = F
  nyd (F f) = f

mul = dyn f
  where
    f (a:b:[]) =
      if nyd (b == (1::Int))
        then a
        else if (_reflect a) P.== "number"
             then mulInt a b
             else if (_reflect a) P.== "string"
                  then mul $$ [ concat a a, decInt b ]
                  else error "'type' error"

a * b = mul $$ [a, b]

main = do
  print (dyn True)
  print (dyn False)
  print (N 123)
  print (dyn "hello")
  print show
  print $ (N 2)      * (N 3)
  print $ (dyn "hi") * (N 3) * (N 2)
  print $ (dyn "hi") * (N 3) * (N 2) * (dyn "abc") -- "type error"
  -- print $ (B True) * (N 3) -- "type error"
