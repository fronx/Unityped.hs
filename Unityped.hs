{-# LANGUAGE FlexibleInstances #-}

module Unityped where

import Prelude hiding ((*), (==), (++), print, show, concat)
import qualified Prelude as P
import Data.List hiding ((++))

data D = B Bool
       | N Int
       | S String
       | F ([D] -> D)
       | List [D]
       | Obj { typeOf :: D
             , fields :: [(String, D)]
             }
       | Class { _name   :: String
               , _constr :: [D] -> [(String, D)]
               }
       | Method
       | Null

typeCase :: D -> [(String, D)] -> D
typeCase d [] = error "empty case list"
typeCase d ((dynType, d'):more)
  | (_reflect d) P.== dynType = d'
  | otherwise = typeCase d more

-- access a member of an object
(.@) :: D -> String -> D
obj .@ field = member (fields obj)
  where
    member [] = Null
    member ((key, val):more) =
      if key P.== field
      then val
        else member more

-- create an instance of a class
new _class args =
  Obj _class ((_constr _class) args)

className obj = dyn (_name (typeOf obj))

show = dyn f
  where f (B True  :[]) = dyn "true"
        f (B False :[]) = dyn "false"
        f (N n     :[]) = dyn (P.show n)
        f (S s     :[]) = dyn s
        f (F _     :[]) = dyn "(function)"
        f (List l  :[]) = (dyn "[") ++ showList nydShow l ++ (dyn "]")
        f (obj     :[]) | (Obj _class _fields) <- obj =
          (className obj) ++
            (dyn "{") ++
            showList pairShow (fields obj) ++
            (dyn "}")
        showList showFn l = (dyn (intercalate ", " (map showFn l)))
        nydShow d = nyd (show $$ [d])
        pairShow (key, value) =
          key P.++ "=" P.++ nyd (show $$ [value])

print d = putStrLn s
  where S s = show $$ [d]

reflect = dyn f
  where f (d:[]) = dyn (_reflect d)

_reflect (B _) = "bool"
_reflect (N _) = "number"
_reflect (S _) = "string"
_reflect (F _) = "function"
_reflect (List _) = "list"

class DynEq a where
  (==) :: D -> a -> D

infix 4 ==

instance DynEq Int where
  (N n) == m = dyn (n P.== m)

instance DynEq String where
  (S s) == s' = dyn (s P.== s')

instance DynEq D where
  (B b) == (B b') = dyn (b P.== b')
  (S s) == (S s') = dyn (s P.== s')
  (N n) == (N n') = dyn (n P.== n')

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

instance Dyn [D] where
  dyn = List
  nyd (List l) = l

dynf :: (Dyn a, Dyn b) => (a -> b) -> D -> D
dynf f d = dyn (f (nyd d))

dynf2 :: (Dyn a, Dyn b, Dyn c) => (a -> b -> c) -> D -> D -> D
dynf2 f d d' = dyn (f (nyd d) (nyd d'))

decInt = dynf f
  where f :: Int -> Int
        f n = n - 1

minusInt = dynf2 f
  where f :: Int -> Int -> Int
        f a b = a - b

mulInt = dynf2 f
  where f :: Int -> Int -> Int
        f = (P.*)

(++) = dynf2 f
  where f :: String -> String -> String
        f = (P.++)

($$) :: D -> [D] -> D
(F f) $$ ds = f ds

mul = dyn f
  where
    f (a:b:[]) =
      if nyd (b == (1::Int))
        then a
        else typeCase a
          [ ("number", mulInt a b)
          , ("string", mul $$ [ a ++ a, decInt b ])
          ]

a * b = mul $$ [a, b]

person = Class "person" constr
  where
    constr (name:birthyear:[]) =
        [ ("name",       name)
        , ("birthyear",  birthyear)
        , ("age",        age)
        ]
      where
        year = N 2014
        age = minusInt year birthyear

mary = new person [ dyn "Mary", N 1978 ]
joe  = new person [ dyn "Joe",  N 1992 ]

main = do
  print (dyn True)
  print (dyn False)
  print (N 123)
  print (dyn "hello")
  print $ (dyn "2 == 3 ") ++ (show $$ [(N 2) == (N 3)])
  print $ (dyn "4 == 2 * 2 ") ++ (show $$ [(N 4) == (N 2) * (N 2)])
  print $ (dyn [ dyn "123", N 123, dyn [ dyn True ] ])
  print show
  print mary
  print (mary .@ "age")
  print joe
  print $ (dyn "reflect(show): ") ++ (reflect $$ [show])
  print $ (N 2)      * (N 3)
  print $ (dyn "hi") * (N 3) * (N 2)
  print $ (dyn "hi") * (N 3) * (N 2) * (dyn "abc") -- "type error"
  -- print $ (B True) * (N 3) -- "type error"
