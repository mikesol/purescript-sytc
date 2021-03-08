module Basic where

import Prelude
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Typeclass (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, uncons, union)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\))

newtype ShowMe a
  = ShowMe (a -> String)

type MyShows
  = TypeclassCons' Int ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')

shower :: forall x head tail row. Cons x ShowMe head tail row => Typeclass row -> x -> String
shower row x =
  let
    (ShowMe f) = fst (uncons (Proxy :: Proxy x) row)
  in
    f x

myShows :: Typeclass MyShows
myShows = cons (Proxy :: Proxy Int) (ShowMe $ show) empty (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)

myShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
myShow = shower myShows

yourShows :: Typeclass MyShows
yourShows = cons (Proxy :: Proxy Int) (ShowMe $ const "Fooled you with a fake integer!") empty (cons (Proxy :: Proxy Boolean) (ShowMe $ show) empty empty)

yourShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
yourShow = shower yourShows

meanShows :: Typeclass MyShows
meanShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) myShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) yourShows
  in
    union h t

meanShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
meanShow = shower meanShows

niceShows :: Typeclass MyShows
niceShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) yourShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) myShows
  in
    union h t

niceShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
niceShow = shower niceShows

basic :: Effect Unit
basic = do
  log $ myShow true
  log $ myShow 1
  log $ yourShow true
  log $ yourShow 1
  log $ niceShow true
  log $ niceShow 1
  log $ meanShow true
  log $ meanShow 1
