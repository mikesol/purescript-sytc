module Step1111Impl where

import Prelude
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Step1Lib (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, uncons, union)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

shower :: forall x head tail row. Cons x ShowMe head tail row => Typeclass row -> x -> String
shower row x =
  let
    (ShowMe f) = fst (uncons (Proxy :: Proxy x) row)
  in
    f x

class ShowPeano (p :: Peano) where
  showPeano :: Proxy p -> String

instance showPeanoZ :: ShowPeano Z where
  showPeano _ = "Z"

instance showPeanoSucc :: ShowPeano x => ShowPeano (Succ x) where
  showPeano _ = "Succ ( " <> (showPeano (Proxy :: Proxy x)) <> ")"

class ShowPeanoAlt (p :: Peano) where
  showPeanoAlt :: Proxy p -> String

instance showPeanoAltZ :: ShowPeanoAlt Z where
  showPeanoAlt _ = "z"

instance showPeanoAltSucc :: ShowPeanoAlt x => ShowPeanoAlt (Succ x) where
  showPeanoAlt _ = "succ ( " <> (showPeanoAlt (Proxy :: Proxy x)) <> ")"

myShows :: forall (p :: Peano). ShowPeano p => Proxy p -> Typeclass (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))
myShows _ =
  cons
    (Proxy :: Proxy (Proxy p))
    (ShowMe showPeano)
    empty
    (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)

yourShows :: forall (p :: Peano). ShowPeano p => ShowPeanoAlt p => Proxy p -> Typeclass (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))
yourShows _ =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy (Proxy p)) (myShows (Proxy :: Proxy p))
  in
    cons (Proxy :: Proxy (Proxy p)) (ShowMe showPeanoAlt) empty t

class AsPeano x (y :: Peano) | x -> y where
  asPeano :: Proxy x -> Proxy y

instance asPeanoSucc :: AsPeano (Succ x) (Succ x) where
  asPeano = identity
else instance asPeanoZ :: AsPeano Z Z where
  asPeano = identity
else instance asPeanoX :: AsPeano x Z where
  asPeano _ = Proxy :: Proxy Z

myShow :: forall (p :: Peano) x head tail. AsPeano x p => ShowPeano p => Cons x ShowMe head tail (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')) => x -> String
myShow x = shower ((myShows :: Proxy p -> Typeclass (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))) (asPeano (Proxy :: Proxy x))) x

{-
class MyShow x where
  myShow :: x -> String

instance myShowP :: (ShowPeano p, Cons (Proxy p) ShowMe head tail (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))) => MyShow (Proxy p) where
  myShow x = shower (myShows x) x
else instance myShowA :: Cons x ShowMe head tail (TypeclassCons' (Proxy Z) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')) => MyShow x where
  myShow x = shower (myShows (Proxy :: Proxy Z)) x
-}
step111 :: Effect Unit
step111 = do
  log $ myShow true
  log $ myShow (Proxy :: Proxy Z)
  --log $ myShow (Proxy :: Proxy (Succ Z))
  pure unit
