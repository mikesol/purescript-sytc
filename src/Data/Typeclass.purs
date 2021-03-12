module Data.Typeclass
  ( Typeclass
  , Typeclass'
  , class Cons
  , cons
  , uncons
  , tnil
  , TypeclassWithUnCons
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
  , class WithUnCons
  , class Inductor
  , class HeadCons
  , class TailCons
  , headCons
  , tailCons
  , induct
  , class UnCons
  , class Union
  , union
  , using'
  , using''
  , consSingleton
  , consViaInduction
  , TNil
  , type (@?)
  , type (@@)
  , type (/@\)
  , type (@>)
  , type (@!>)
  , (@>)
  , (-@-)
  , (@!>)
  , (-@)
  , (@-)
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

class HeadCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class HeadCons label func head row | label row -> func head where
  headCons :: Proxy label -> Typeclass row -> func label /\ Typeclass head

class HeadConsChain :: forall (l :: Type). TypeclassRow' -> (l -> Type) -> Typeclass' -> Constraint
class HeadConsChain labels func row


instance headConsChainN :: HeadConsChain TypeclassNil' f row

instance headConsChainC :: (HeadCons h f x row, HeadConsChain t f row) => HeadConsChain (TypeclassCons' h t) f row

instance headConsCacheHitS :: HeadCons l f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  headCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (unsafeCoerce a') /\ Typeclass Nil
  headCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance headConsCacheHitG :: Inductor f (gen x) l gen  => HeadCons (Proxy (gen x)) f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  headCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy (gen x) -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy (gen x))) (Proxy :: Proxy (gen x)) (unsafeCoerce a'))) /\ Typeclass Nil
  headCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance headConsCacheHitZ :: Inductor f l l gen => HeadCons (Proxy l) f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  headCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy l -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy l)) (Proxy :: Proxy l) (unsafeCoerce a'))) /\ Typeclass Nil
  headCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance headConsCacheMiss :: HeadCons l f (TypeclassC' f notC) (TypeclassC' f c) => HeadCons l f (TypeclassC' f (TypeclassCons' x'x notC)) (TypeclassC' f (TypeclassCons' x'x c)) where
  headCons l (Typeclass (a : b)) = let x /\ (Typeclass y) = (headCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass (TypeclassC' f notC)) l (Typeclass b) in x /\ (Typeclass (a : y))
  headCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class TailCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class TailCons label func tail row | label row -> func tail where
  tailCons :: Proxy label -> Typeclass row -> func label /\ Typeclass tail

instance tailConsCacheHit :: TailCons l f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  tailCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (unsafeCoerce a') /\ Typeclass b
  tailCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance tailConsCacheHitS :: Inductor f (gen x) l gen => TailCons (Proxy (gen x)) f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  tailCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy (gen x) -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy (gen x))) (Proxy :: Proxy (gen x)) (unsafeCoerce a'))) /\ Typeclass b
  tailCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance tailConsCacheHitZ :: Inductor f l l gen => TailCons (Proxy l) f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassViaInduction' l gen) c)) where
  tailCons _ (Typeclass (a : b)) =
    let (TypeclassType a') = unsafeCoerce a
    in
      (((induct :: Proxy l -> f (Proxy l) /\ (forall y. Proxy y -> f (Proxy y) -> f (Proxy (gen y))) -> f (Proxy l)) (Proxy :: Proxy l) (unsafeCoerce a'))) /\ Typeclass b
  tailCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance tailConsCacheMiss :: TailCons l f o (TypeclassC' f c) => TailCons l f o (TypeclassC' f (TypeclassCons' x'x c)) where
  tailCons l (Typeclass (a : b)) = (tailCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass o) l (Typeclass b)
  tailCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class Cons :: forall (l :: Type). TypeclassType' -> Typeclass' -> Typeclass' -> Constraint
class Cons a tail row | a tail -> row where
  cons :: TypeclassType a -> Typeclass tail -> Typeclass row

instance consS :: Cons (TypeclassSingleton' label) (TypeclassC' func tail) (TypeclassC' func (TypeclassCons' (TypeclassSingleton' label) tail)) where
  cons a (Typeclass t) = Typeclass ((unsafeCoerce a) : t)
else instance consI :: Cons (TypeclassViaInduction' label gen)  (TypeclassC' func tail) (TypeclassC' func (TypeclassCons' (TypeclassViaInduction' label gen) tail)) where
  cons a (Typeclass t) = Typeclass ((unsafeCoerce a) : t)

class WithUnCons :: forall (l :: Type). TypeclassRow' -> (l -> Type) ->  Typeclass' -> Constraint
class WithUnCons labels func row | labels func -> row

instance withUnConsN :: WithUnCons TypeclassNil' x row
instance withUnConsC :: (WithUnCons b x (TypeclassC' x r), UnCons a x h t (TypeclassC' x r)) => WithUnCons (TypeclassCons' a b) x (TypeclassC' x r)

type TypeclassWithUnCons :: forall k. (k -> Type) -> TypeclassRow' -> Type
type TypeclassWithUnCons func labels = forall row. WithUnCons labels func row => Typeclass row
infixr 6 type TypeclassWithUnCons as @?

class UnCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class UnCons label func head tail row | label row -> func head tail where
  uncons :: Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance consTypeclassUnCons' :: (TailCons label func tail row, HeadCons label func head row) => UnCons label func head tail row where
  uncons a b =
    let
      f /\ h = headCons a b
      _ /\ t = tailCons a b
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

using'' :: forall x f head tail row g. UnCons x f head tail (TypeclassC' f row) => (f x -> g) -> Typeclass (TypeclassC' f row) -> g
using'' f row = f $ fst (uncons (Proxy :: Proxy x) row)


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
infixr 5 uncons as -@-
infixr 5 headCons as -@
infixr 5 tailCons as @-
infixr 5 union as <@@>
