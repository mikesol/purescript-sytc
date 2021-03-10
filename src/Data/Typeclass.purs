module Data.Typeclass
  ( Typeclass
  , Typeclass'
  , class Cons
  , cons
  , uncons
  , tnil
  , TypeclassType
  , TypeclassC'
  , TypeclassRow'
  , TypeclassNil'
  , TypeclassCons'
  , TypeclassType'
  , TypeclassSingleton'
  , TypeclassViaInduction'
  , TypeclassConsSingleton
  , TypeclassCons
  , class Inductor
  , class NegCons
  , class PosCons
  , negCons
  , posCons
  , induct
  , class UnCons
  , class Union
  , union
  , using'
  , consSingleton
  , consViaInduction
  , TNil
  , type (@@)
  , type (/@\)
  , type (@>)
  , type (@!>)
  , (@>)
  , (@-)
  , (@!>)
  , (<@@>)
  , using
  , using_
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data TypeclassRow'

data TypeclassType'

foreign import data TypeclassSingleton' :: forall k. k -> TypeclassType'

foreign import data TypeclassViaInduction' :: forall k. k -> (k -> k) -> TypeclassType'

foreign import data TypeclassCons' :: TypeclassType' -> TypeclassRow' -> TypeclassRow'
foreign import data TypeclassNil' :: TypeclassRow'

foreign import data TypeclassC' :: forall k. (k -> Type) -> TypeclassRow' -> Typeclass'

data Typeclass'

infixr 5 type TypeclassC' as @@
infixr 5 type TypeclassViaInduction' as /@\

type TypeclassConsSingleton :: forall k1. k1 -> TypeclassRow' -> TypeclassRow'
type TypeclassConsSingleton k r = TypeclassCons' (TypeclassSingleton' k) r
infixr 6 type TypeclassConsSingleton as @>

type TypeclassCons ::  TypeclassType' -> TypeclassRow' -> TypeclassRow'
type TypeclassCons k r = TypeclassCons' k r

infixr 6 type TypeclassCons as @!>
type TNil = TypeclassNil'

newtype Typeclass (c :: Typeclass') = Typeclass (List Void)
newtype TypeclassType (t :: TypeclassType') = TypeclassType Void

instance semigroupTypeclassNil' :: Semigroup (Typeclass (TypeclassC' a TypeclassNil')) where
 append _ _ = Typeclass Nil

instance monoidTypeclassNil' :: Monoid (Typeclass (TypeclassC' a TypeclassNil')) where
 mempty = Typeclass Nil

tnil :: forall f. Typeclass (TypeclassC' f TypeclassNil')
tnil = mempty

class Inductor :: forall k. (Type -> Type) -> k -> k -> (k -> k) -> Constraint
class Inductor func target start next  where
  induct :: Proxy target -> func (Proxy start) /\ (forall x. Proxy x -> func (Proxy x) -> func (Proxy (next x))) -> func (Proxy target)
instance inductS ::  Inductor func x start next => Inductor func (next x) start next where
  induct _ store = (snd store) (Proxy :: Proxy x) (induct (Proxy :: Proxy x) store)
else instance inductZ :: Inductor func start start next where
  induct _ = fst

class NegCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class NegCons label func head row | label row -> func head where
  negCons :: Proxy label -> Typeclass row -> func label /\ Typeclass head

instance negConsCacheHitS :: NegCons l f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  negCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (unsafeCoerce a') /\ Typeclass Nil
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheHitG :: Inductor f (gen x) l gen  => NegCons (Proxy (gen x)) f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  negCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy (gen x) -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy (gen x))) (Proxy :: Proxy (gen x)) (unsafeCoerce a'))) /\ Typeclass Nil
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheHitZ :: Inductor f l l gen => NegCons (Proxy l) f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  negCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy l -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy l)) (Proxy :: Proxy l) (unsafeCoerce a'))) /\ Typeclass Nil
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheMiss :: NegCons l f (TypeclassC' f notC) (TypeclassC' f c) => NegCons l f (TypeclassC' f (TypeclassCons' x'x notC)) (TypeclassC' f (TypeclassCons' x'x c)) where
  negCons l (Typeclass (a : b)) = let x /\ (Typeclass y) = (negCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass (TypeclassC' f notC)) l (Typeclass b) in x /\ (Typeclass (a : y))
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class PosCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class PosCons label func tail row | label row -> func tail where
  posCons :: Proxy label -> Typeclass row -> func label /\ Typeclass tail

instance posConsCacheHit :: PosCons l f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  posCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (unsafeCoerce a') /\ Typeclass b
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance posConsCacheHitS :: Inductor f (gen x) l gen => PosCons (Proxy (gen x)) f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  posCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy (gen x) -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy (gen x))) (Proxy :: Proxy (gen x)) (unsafeCoerce a'))) /\ Typeclass b
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance posConsCacheHitZ :: Inductor f l l gen => PosCons (Proxy l) f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  posCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy l -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy l)) (Proxy :: Proxy l) (unsafeCoerce a'))) /\ Typeclass b
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance posConsCacheMiss :: PosCons l f o (TypeclassC' f c) => PosCons l f o (TypeclassC' f (TypeclassCons' x'x c)) where
  posCons l (Typeclass (a : b)) = (posCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass o) l (Typeclass b)
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class Cons :: forall (l :: Type). TypeclassType' -> Typeclass' -> Typeclass' -> Constraint
class Cons a tail row | a tail -> row where
  cons :: TypeclassType a -> Typeclass tail -> Typeclass row

instance consS :: Cons (TypeclassSingleton' label) (TypeclassC' func tail) (TypeclassC' func (TypeclassCons' (TypeclassSingleton' label) tail)) where
  cons a (Typeclass t) = Typeclass ((unsafeCoerce a) : t)
else instance consI :: Cons (TypeclassViaInduction' label gen)  (TypeclassC' func tail) (TypeclassC' func (TypeclassCons' (TypeclassViaInduction' label gen) tail)) where
  cons a (Typeclass t) = Typeclass ((unsafeCoerce a) : t)

class UnCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class UnCons label func head tail row | label row -> func head tail where
  uncons :: Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance consTypeclassUnCons' :: (PosCons label func tail row, NegCons label func head row) => UnCons label func head tail row where
  uncons a b =
    let
      f /\ h = negCons a b
      _ /\ t = posCons a b
    in
      f /\ h /\ t

class Union :: Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Union l r row | l r -> row where
  union :: Typeclass l -> Typeclass r -> Typeclass row

instance unionTypeclassNil' :: Union (TypeclassC' c TypeclassNil') (TypeclassC' c x) (TypeclassC' c x) where
  union _ = identity

instance unionTypeclassCons' :: Union (TypeclassC' f a) b (TypeclassC' f o) => Union (TypeclassC' f (TypeclassCons' k a)) b (TypeclassC' f (TypeclassCons' k o)) where
  union (Typeclass a) (Typeclass b) = Typeclass (a <> b)

using :: forall x f f' head tail row. Newtype (f x) f' => UnCons x f head tail (TypeclassC' f row) => Typeclass (TypeclassC' f row) -> f'
using row = unwrap $ fst (uncons (Proxy :: Proxy x) row)

using' :: forall x f head tail row. UnCons x f head tail (TypeclassC' f row) => Typeclass (TypeclassC' f row) -> f x
using' row = fst (uncons (Proxy :: Proxy x) row)

using_ :: forall x f f' head tail row. Newtype (f x) f' => UnCons x f head tail (TypeclassC' f row) => (Unit -> Typeclass (TypeclassC' f row) )-> f'
using_ row = unwrap $ fst (uncons (Proxy :: Proxy x) (row unit))


consSingleton ::
  forall label func tail.
  Cons
    (TypeclassSingleton' label)
    (TypeclassC' func tail)
    (
      TypeclassC'
      func
      (TypeclassCons' (TypeclassSingleton' label) tail)
    ) =>
  func label ->
  Typeclass (TypeclassC' func tail) ->
  Typeclass (TypeclassC' func (TypeclassCons' (TypeclassSingleton' label) tail))
consSingleton a b = (cons :: (TypeclassType (TypeclassSingleton' label)) -> Typeclass (TypeclassC' func tail) -> Typeclass (TypeclassC' func (TypeclassCons' (TypeclassSingleton' label) tail))) (TypeclassType (unsafeCoerce a)) b

consViaInduction ::
  forall sko start next func tail.
  Cons
    (TypeclassViaInduction' start next)
    (TypeclassC' func tail)
    (
      TypeclassC'
      func
      (TypeclassCons' (TypeclassViaInduction' start next) tail)
    ) =>
  func (Proxy start) /\ (Proxy sko -> func (Proxy sko) -> func (Proxy (next sko))) ->
  Typeclass (TypeclassC' func tail) ->
  Typeclass (TypeclassC' func (TypeclassCons' (TypeclassViaInduction' start next) tail))
consViaInduction a b = (cons :: (TypeclassType (TypeclassViaInduction' start next)) -> Typeclass (TypeclassC' func tail) -> Typeclass (TypeclassC' func (TypeclassCons' (TypeclassViaInduction' start next) tail))) (TypeclassType (unsafeCoerce a)) b

infixr 5 consSingleton as @>
infixr 5 consViaInduction as @!>
infixr 5 uncons as @-
infixr 5 union as <@@>
