module Step111Impl where

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

class ShowPeano (p :: Peano) where
  showPeano :: Proxy p -> String

instance showPeanoZ :: ShowPeano Z where
  showPeano _ = "Z"

instance showPeanoSucc :: ShowPeano x => ShowPeano (Succ x) where
  showPeano _ = "Succ ( " <> (showPeano (Proxy :: Proxy x)) <> ")"

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

myShows :: forall (p :: Peano). ShowPeano p => Proxy p -> Typeclass (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))
myShows _ =
  cons
    (Proxy :: Proxy (Proxy p))
    (ShowMe showPeano)
    empty
    (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)

myShow :: forall x head tail. Cons x ShowMe head tail (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')) => x -> String
myShow = shower (myShows (Proxy :: Proxy Z))

myShowP :: forall (p :: Peano) head tail. ShowPeano p => Cons (Proxy p) ShowMe head tail (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')) => Proxy p -> String
myShowP x = shower (myShows x) x

step111 :: Effect Unit
step111 = do
  log $ myShow true
  log $ myShowP (Proxy :: Proxy Z)
  log $ myShowP (Proxy :: Proxy (Succ Z))
  pure unit
