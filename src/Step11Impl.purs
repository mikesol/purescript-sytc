module Step11Impl where

import Prelude
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class.Console (log)
import Step1Lib (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, uncons, union)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

showPeanoZ :: Proxy Z -> String
showPeanoZ _ = "Z"

class Prev a b | a -> b, b -> a

instance prevPeanoZ :: Prev Z Z
else instance prevPeanoS :: Prev (Succ x) x

newtype ShowMe a
  = ShowMe (a -> String)

shower :: forall x head tail row. Cons x ShowMe head tail row => Typeclass row -> x -> String
shower row x =
  let
    (ShowMe f) = fst (uncons (Proxy :: Proxy x) row)
  in
    f x

myShows :: forall (p :: Peano) (p' :: Peano) head tail. Prev p p' => Cons (Proxy p') ShowMe head tail (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' (Proxy p') ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))) => Typeclass (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')))
myShows =
  cons
    (Proxy :: Proxy (Proxy Z))
    (ShowMe $ showPeanoZ)
    empty
    ( cons
        (Proxy :: Proxy (Proxy p))
        ( ShowMe
            -- we can't walk backwards through prev
            
            -- unable to solve for it, which makes sense
            
            -- we have no clue which branch our p' should go down
            
            $ \_ -> "" -- (shower :: Typeclass (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' (Proxy p') ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))) -> (Proxy p') -> String) (myShows :: Typeclass (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' (Proxy p') ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')))) (Proxy :: Proxy p')
        )
        empty
        (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)
    )

--step11 :: Effect Unit
--step11 = do
--  log $ shower true
--  log $ shower 1
