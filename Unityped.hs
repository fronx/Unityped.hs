{-# LANGUAGE FlexibleInstances #-}
module Unityped where

-- hide builtin functions so we can use our own
-- implementations instead:
import Prelude hiding ((*), (-), (==), (++), print, show, concat)
-- if we want to use builtins, we just write P.<builtin>
import qualified Prelude as P
import Data.List (intercalate)
import Debug.Trace

-- This is the one static type that contains
-- all the "dynamic types":
data D = B Bool
       | N Int
       | S String
       | F ([D] -> D)
       | List [D]
       -- OMG it's even object-oriented!
       | Class { _name   :: String
               , _constr :: [D] -> [(String, D)]
               }
       | Obj { typeOf :: D
             , fields :: [(String, D)]
             }
       | Null -- YOLO

-- operator for accessing a member of an object
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

-- runtime type error
typeError expectedType receivedD =
  traceStack msg (error "typeError")
    where msg = "!! typeError: expected a " P.++ expectedType P.++
                " but received a " P.++ (_reflect receivedD) P.++
                ": " P.++ nyd (show $$ [receivedD])

-- this set of polymorphic functions is the connection
-- between the typed world and the dynamically typed world.
-- you can implement these functions for any static type
-- in order to make it participate.
class Dyn a where
  -- converting static to dynamic
  dyn :: a -> D
  -- converting dynamic to static
  nyd :: D -> a
  -- you can compare a dynamic value with a typed value
  (==) :: D -> a -> D
  -- explicitly empty default implementation of equality:
  d == _ = undefined

-- operator precedence for equality
infix 4 ==

--
-- and here we go, adding types to the world of Dyns:
--
instance Dyn Bool where
  dyn = B
  nyd (B b)   = b
  nyd d       = typeError "bool" d
  (B b) == b' = dyn (b P.== b')
  d     == _  = typeError "bool" d

instance Dyn Int where
  dyn = N
  nyd (N n)  = n
  nyd d      = typeError "number" d
  (N n) == m = dyn (n P.== m)
  d     == _ = typeError "number" d

instance Dyn String where
  dyn = S
  nyd (S s)   = s
  nyd d       = typeError "string" d
  (S s) == s' = dyn (s P.== s')
  d     == _  = typeError "string" d

instance Dyn ([D] -> D) where
  dyn = F
  nyd (F f) = f
  nyd d     = typeError "function" d

instance Dyn [D] where
  dyn = List
  nyd (List l) = l
  nyd d = typeError "list" d

instance Dyn D where
  dyn d = d
  nyd d = d
  (B b) == (B b') = dyn (b P.== b')
  (S s) == (S s') = dyn (s P.== s')
  (N n) == (N n') = dyn (n P.== n')

--
-- amazing helper functions
--

-- turn a proper unary function into a dynamic function
dynf :: (Dyn a, Dyn b) => (a -> b) -> D -> D
dynf f d = dyn (f (nyd d))

-- turn a proper binary function into a dynamic function
dynf2 :: (Dyn a, Dyn b, Dyn c) => (a -> b -> c) -> D -> D -> D
dynf2 f d d' = dyn (f (nyd d) (nyd d'))

decInt = dynf f
  where f :: Int -> Int
        f n = n P.- 1

mulInt = dynf2 f
  where f :: Int -> Int -> Int
        f = (P.*)

-- operator for concatenation of dynamic strings
(++) = dynf2 f
  where f :: String -> String -> String
        f = (P.++)

-- operator for applying a dynamic function to its arguments
($$) :: D -> [D] -> D
(F f) $$ ds = f ds

-- this is a convenience function that allows you
-- to dispatch on a string representation of the
-- "dynamic type" of a value:
typeCase :: D -> [(String, D)] -> D
typeCase d [] = typeError "(?)" d
typeCase d ((dynType, d'):more)
  | (_reflect d) P.== dynType = d'
  | otherwise = typeCase d more

-- turning a dynamic value into a dynamic string
show = dyn f
  where
    f (B True  :[]) = dyn "true"
    f (B False :[]) = dyn "false"
    f (N n     :[]) = dyn (P.show n)
    f (S s     :[]) = dyn s
    f (F _     :[]) = dyn "(function)"
    f (Null    :[]) = dyn "(null)"
    f (List l  :[]) = (dyn "[") ++ showList nydShow l ++ (dyn "]")
    f (obj     :[]) | (Obj _class _fields) <- obj =
      (className obj) ++
        (dyn "{") ++
        showList pairShow (fields obj) ++
        (dyn "}")
    showList showFn l = (dyn (Data.List.intercalate ", " (map showFn l)))
    nydShow d = nyd (show $$ [d])
    pairShow (key, value) =
      key P.++ "=" P.++ nyd (show $$ [value])

-- runtime type reflection
reflect = dyn f
  where f (d:[]) = dyn (_reflect d)

-- low-level runtime type reflection
_reflect (B _) = "bool"
_reflect (N _) = "number"
_reflect (S _) = "string"
_reflect (F _) = "function"
_reflect (List _) = "list"
_reflect obj | (Obj _ _) <- obj = _name (typeOf obj)

-- the name of the class of an object as a dynamic string
className = dyn . _reflect

-- multiplication for dynamic numbers and strings
mul = dyn f
  where
    f (a:b:[]) =
      if nyd (b == (1::Int))
        then a
        else typeCase a
          [ ("number", mulInt a b)
          , ("string", mul $$ [ a ++ a, decInt b ])
          ]

minus = dyn f
  where
    f (a:b:[]) = N ((nyd a) P.- (nyd b))

-- operator syntax for convenience
a * b = mul $$ [a, b]
a - b = minus $$ [a, b]

-- hey, let's define a class!
person = Class "person" constr
  where
    constr (name:birthyear:[]) =
        [ ("name",       name)
        , ("birthyear",  birthyear)
        , ("age",        age)
        ]
      where
        year = N 2014
        age = year - birthyear

-- and some objects of our class!
mary = new person [ dyn "Mary", N 1978 ]
joe  = new person [ dyn "Joe",  N 1992 ]
nobody = new person [ dyn "Nobody", dyn "1884" ]

--
-- okay, let's test a few things, including runtime errors
--
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
  print nobody -- runtime type error
  print $ (dyn "reflect(show): ") ++ (reflect $$ [show])
  print $ (N 2)      * (N 3)
  print $ (dyn "hi") * (N 3) * (N 2)
  print $ (dyn "hi") * (N 3) * (N 2) * (dyn "abc") -- runtime type error
  print $ (B True) * (N 3) -- runtime type error

  where
    -- and another convenience function
    print d = putStrLn s
      where S s = show $$ [d]
