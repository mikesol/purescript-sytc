module Step1111Impl where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Step1Lib (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, get, uncons)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

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

class AsPeano :: forall k. k -> Peano -> Constraint
class AsPeano x (y :: Peano) | x -> y where
  asPeano :: Proxy x -> Proxy y

instance asPeanoSucc :: AsPeano (Proxy (Succ x)) (Succ x) where
  asPeano _ = Proxy :: Proxy (Succ x)
else instance asPeanoZ :: AsPeano (Proxy Z) Z where
  asPeano _ = Proxy :: Proxy Z
else instance asPeanoX :: AsPeano x Z where
  asPeano _ = Proxy :: Proxy Z

type BaseShow (p :: Peano) = TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')

type MyShows (p :: Peano) = ShowPeano p => Proxy p -> Typeclass (BaseShow p)

myShows :: forall (p :: Peano). MyShows p
myShows _ =
  cons
    (Proxy :: Proxy (Proxy p))
    (ShowMe showPeano)
    empty
    (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)

type YourShows (p :: Peano) = ShowPeano p => ShowPeanoAlt p => Proxy p -> Typeclass (TypeclassCons' (Proxy p) ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))

yourShows :: forall (p :: Peano). YourShows p
yourShows _ =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy (Proxy p)) (myShows (Proxy :: Proxy p))
  in
    cons (Proxy :: Proxy (Proxy p)) (ShowMe showPeanoAlt) empty t

myShow :: forall (p :: Peano) x head tail. AsPeano x p => ShowPeano p => Cons x ShowMe head tail (BaseShow p) => x -> String
myShow = get (Proxy :: Proxy ShowMe) ((myShows :: MyShows p) (asPeano (Proxy :: Proxy x)))

yourShow :: forall (p :: Peano) x head tail. AsPeano x p => ShowPeano p => ShowPeanoAlt p => Cons x ShowMe head tail (BaseShow p) => x -> String
yourShow = get (Proxy :: Proxy ShowMe) ((yourShows :: YourShows p) (asPeano (Proxy :: Proxy x)))

step1111 :: Effect Unit
step1111 = do
  log $ myShow true
  log $ myShow (Proxy :: Proxy Z)
  log $ myShow (Proxy :: Proxy (Succ Z))
  log $ yourShow true
  log $ yourShow (Proxy :: Proxy Z)
  log $ yourShow (Proxy :: Proxy (Succ Z))
  pure unit
