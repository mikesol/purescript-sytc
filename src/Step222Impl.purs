module Step222Impl where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Step22Lib (class Cons, TypePairC', Typeclass, TypeclassCons', TypeclassNil', cons, empty, uncons, union)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

--data PProxy (p :: Peano)
--  = PProxy
--instance showPeanoZ :: Show (PProxy Z) where
--  show _ = "Z"
--instance showPeanoSucc :: Show (PProxy x) => Show (PProxy (Succ x)) where
--  show _ = "Succ (" <> show (PProxy :: PProxy x) <> ")"
newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type TPPeano p
  = (TypePairC' (Proxy p) ShowMe)

shower :: forall x head tail row. Cons (TypePairC' x ShowMe) x ShowMe head tail row => Typeclass row -> x -> String
shower row x =
  let
    (ShowMe f) = fst (uncons (Proxy :: Proxy (TypePairC' x ShowMe)) (Proxy :: Proxy x) row)
  in
    f x

myShows :: forall p h t. Cons (TypePairC' (Proxy p) ShowMe) (Proxy p) ShowMe h t (TypeclassCons' (TypePairC' (Proxy Z) ShowMe) (TypeclassCons' (TypePairC' (Proxy p) ShowMe) (TypeclassCons' (TypePairC' Boolean ShowMe) TypeclassNil'))) => Typeclass (TypeclassCons' (TypePairC' (Proxy Z) ShowMe) (TypeclassCons' (TypePairC' (Proxy (Succ p)) ShowMe) (TypeclassCons' (TypePairC' Boolean ShowMe) TypeclassNil')))
myShows =
  cons
    (Proxy :: Proxy (TypePairC' (Proxy Z) ShowMe))
    (Proxy :: Proxy (Proxy Z))
    (ShowMe $ \_ -> "Z")
    empty
    -- problem is that Succ x appears in the type signature, so it can't figure this out...
    ( cons (Proxy :: Proxy (TypePairC' (Proxy (Succ p)) ShowMe))
        (Proxy :: Proxy (Proxy (Succ p)))
        (ShowMe $ \_ -> "") -- (shower :: Typeclass (TypeclassCons' (TypePairC' (Proxy Z) ShowMe) (TypeclassCons' (TypePairC' (Proxy p) ShowMe) (TypeclassCons' (TypePairC' Boolean ShowMe) TypeclassNil'))) -> (Proxy p) -> String) (myShows :: (Typeclass (TypeclassCons' (TypePairC' (Proxy Z) ShowMe) (TypeclassCons' (TypePairC' (Proxy p) ShowMe) (TypeclassCons' (TypePairC' Boolean ShowMe) TypeclassNil'))))) (Proxy :: Proxy p))
        empty
        (cons (Proxy :: Proxy (TypePairC' Boolean ShowMe)) (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)
    )

{-
yourShows :: Typeclass MyShows
yourShows = cons (Proxy :: forall (p :: Peano). Proxy (PProxy p)) (ShowMe $ const "Fooled you with a fake integer!") empty (cons (Proxy :: Proxy Boolean) (ShowMe $ show) empty empty)

yourShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
yourShow = shower yourShows

meanShows :: Typeclass MyShows
meanShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: forall (p :: Peano). Proxy (PProxy p)) myShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) yourShows
  in
    union h t

meanShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
meanShow = shower meanShows

niceShows :: Typeclass MyShows
niceShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: forall (p :: Peano). Proxy (PProxy p)) yourShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) myShows
  in
    union h t

niceShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
niceShow = shower niceShows

step1 :: Effect Unit
step1 = do
  log $ myShow true
  log $ myShow 1
  log $ yourShow true
  log $ yourShow 1
  log $ niceShow true
  log $ niceShow 1
  log $ meanShow true
  log $ meanShow 1
-}
--step22 :: Effect Unit
--step22 = do
--  log $ (unwrap $ fst (uncons (Proxy :: Proxy (TypePairC' Boolean ShowMe)) (Proxy :: Proxy Boolean) myShows)) true
