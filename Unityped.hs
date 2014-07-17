{-# LANGUAGE FlexibleInstances #-}
module Unityped where

-- hide builtin functions so we can use our own
-- implementations instead:
import Prelude hiding ((*), (-), (==), (++), (<), (>), print, show, concat)
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
       | Obj { _typeOf :: D
             , _fields :: [(String, D)]
             }
       | Null -- YOLO

-- operator for accessing a member of an object
infix 9 .@ -- strongly binding
(.@) :: D -> String -> D
obj .@ field = member (_fields obj)
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

_decInt = dynf f
  where f :: Int -> Int
        f n = n P.- 1

_mulInt = dynf2 f
  where f :: Int -> Int -> Int
        f = (P.*)

-- operator for concatenation of dynamic strings
(++) = dynf2 f
  where f :: String -> String -> String
        f = (P.++)

-- operator for applying a dynamic function to its arguments
infix 3 $$
($$) :: D -> [D] -> D
(F f) $$ ds = f ds


-- this is a convenience function that allows you
-- to dispatch on a string representation of the
-- "dynamic type" of a value:
typeCase :: D -> [(String, D)] -> D -> D
typeCase d [] fallback = fallback
typeCase d ((dynType, d'):more) fallback
  | (_reflect d) P.== dynType = d'
  | otherwise                 = typeCase d more fallback

-- runtime type reflection
reflect = dyn f
  where f (d:[]) = dyn (_reflect d)

-- low-level runtime type reflection
_reflect (B _) = "bool"
_reflect (N _) = "number"
_reflect (S _) = "string"
_reflect (F _) = "function"
_reflect Null  = "null"
_reflect (List _) = "list"
_reflect (Class _ _) = "class"
_reflect obj | (Obj _ _) <- obj = _name (_typeOf obj)

-- the name of the class of an object as a dynamic string
className = reflect

-- turning a dynamic value into a dynamic string.
--
-- we could use pattern matching here instead, but `typeCase`
-- frees us from having to know how dynamic types are represented
-- in the typed world.
show = dyn f
  where
    f (d:[]) = typeCase d
      [ ("bool",     if nyd (d == True) then (dyn "true") else (dyn "false"))
      , ("number",   showNum d)
      , ("string",   nyd d)
      , ("function", dyn "(function)")
      , ("list",     (dyn "[") ++ showList nydShow (nyd d) ++ (dyn "]"))
      , ("null",     dyn "(null)")
      , ("class",    (dyn "class:") ++ (dyn (_name d)))
      ]
      -- otherwise: it must be an instance of a user-defined class
      (showObj d)

    showObj d = ( (className $$ [d])            ++ (dyn "{") ++
                  showList pairShow (_fields d) ++ (dyn "}")
                )
    -- the builtin `show` function is polymorphic and open, i.e.
    -- you can add implementations for any concrete type.
    -- we have to provide an explicit type here to indicate which
    -- instance we want to use. (this is kind of a special situation.)
    showNum = dynf (P.show :: Int    -> String)
    showList showFn l = (dyn (Data.List.intercalate ", " (map showFn l)))
    nydShow d = nyd (show $$ [d])
    pairShow (key, value) = key P.++ "=" P.++ nyd (show $$ [value])

-- multiplication for dynamic numbers and strings
mul = dyn f
  where
    f (a:b:[]) =
      if nyd (b == (1::Int))
        then a
        else typeCase a
          [ ("number", _mulInt a b)
          , ("string", mul $$ [ a ++ a, _decInt b ])
          ]
          (typeError "number or string" a)

minus = dyn f
  where f (a:b:[]) = N ((nyd a) P.- (nyd b))

lt = dyn f
  where
    f (a:b:[]) = dyn (_ltInt (nyd a) (nyd b))
    _ltInt :: Int -> Int -> Bool
    _ltInt a b = a P.< b

gt = dyn f
  where
    f (a:b:[]) = dyn (_gtInt (nyd a) (nyd b))
    _gtInt :: Int -> Int -> Bool
    _gtInt a b = a P.> b

-- operator syntax for convenience
a * b = mul   $$ [a, b]
a - b = minus $$ [a, b]
a < b = lt    $$ [a, b]
a > b = gt    $$ [a, b]

-- hey, let's define a class!
person = Class "person" constr
  where
    constr (name:birthyear:[]) =
        [ ("name",       name)
        , ("birthyear",  birthyear)
        , ("age",        age)
        , ("isOlder",    dyn isOlder)
        ]
      where
        year = N 2014
        age = year - birthyear
        isOlder (other:[]) = dyn (age > (other .@ "age"))

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
  print $ (dyn "Is Joe older than Mary? ") ++ (show $$ [joe .@ "isOlder" $$ [mary]])
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
