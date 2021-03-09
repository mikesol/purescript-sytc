module Data.Typeclass
  ( Typeclass
  , Typeclass'
  , class Cons
  , cons
  , uncons
  , tnil
  , TypeclassC'
  , TypeclassRow'
  , TypeclassNil'
  , TypeclassCons'
  , TypeclassType'
  , TypeclassSingleton'
  , TypeclassStream'
  , TypeclassConsSingleton
  , class NegCons
  , class PosCons
  , negCons
  , posCons
  , class Union
  , union
  , conz
  , TNil
  , type (@@)
  , type (@>)
  , (@>)
  , (@-)
  , (<@@>)
  , using
  , using_
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data TypeclassRow'

data TypeclassType'

foreign import data TypeclassSingleton' :: forall k. k -> TypeclassType'

-- initial generator indexedrow
foreign import data TypeclassStream' :: forall k. k -> (k -> k) -> (k -> TypeclassRow') -> TypeclassType'

foreign import data TypeclassCons' :: TypeclassType' -> TypeclassRow' -> TypeclassRow'
foreign import data TypeclassNil' :: TypeclassRow'

foreign import data TypeclassC' :: forall k. (k -> Type) -> TypeclassRow' -> Typeclass'

data Typeclass'

infixr 5 type TypeclassC' as @@

-- type TypeclassConsSingleton :: forall k. k -> TypeclassRow' -> TypeclassRow'
type TypeclassConsSingleton k r = TypeclassCons' (TypeclassSingleton' k) r
infixr 6 type TypeclassConsSingleton as @>
type TNil = TypeclassNil'

newtype Typeclass (c :: Typeclass') = Typeclass (List Void)

instance semigroupTypeclassNil' :: Semigroup (Typeclass (TypeclassC' a TypeclassNil')) where
 append _ _ = Typeclass Nil

instance monoidTypeclassNil' :: Monoid (Typeclass (TypeclassC' a TypeclassNil')) where
 mempty = Typeclass Nil

tnil :: forall f. Typeclass (TypeclassC' f TypeclassNil')
tnil = mempty

----------------------
--------------
------
---
-- stuck here
-- interesting problem: let's say that we get to a branch
-- it means that we will be pattern matching EITHER against the generator OR against the original
---------------------------------
class NegCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class NegCons label func head row | label row -> func head where
  negCons :: Proxy label -> Typeclass row -> func label /\ Typeclass head

instance negConsCacheHit :: NegCons l f (TypeclassC' f TypeclassNil') (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  negCons _ (Typeclass (a : b)) = unsafeCoerce a /\ Typeclass Nil
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheMiss :: NegCons l f (TypeclassC' f notC) (TypeclassC' f c) => NegCons l f (TypeclassC' f (TypeclassCons' (TypeclassSingleton' notL) notC)) (TypeclassC' f (TypeclassCons' (TypeclassSingleton' notL) c)) where
  negCons l (Typeclass (a : b)) = let x /\ (Typeclass y) = (negCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass (TypeclassC' f notC)) l (Typeclass b) in x /\ (Typeclass (a : y))
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class PosCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Constraint
class PosCons label func tail row | label row -> func tail where
  posCons :: Proxy label -> Typeclass row -> func label /\ Typeclass tail

instance posConsCacheHit :: PosCons l f (TypeclassC' f c) (TypeclassC' f (TypeclassCons' (TypeclassSingleton' l) c)) where
  posCons _ (Typeclass (a : b)) = unsafeCoerce a /\ Typeclass b
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance posConsCacheMiss :: PosCons l f o (TypeclassC' f c) => PosCons l f o (TypeclassC' f (TypeclassCons' (TypeclassSingleton' notL) c)) where
  posCons l (Typeclass (a : b)) = (posCons :: Proxy l -> Typeclass (TypeclassC' f c) -> f l /\ Typeclass o) l (Typeclass b)
  posCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class Cons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Cons label func head tail row | label func head tail -> row, label row -> func head tail where
  cons :: func label -> Typeclass head -> Typeclass tail -> Typeclass row
  uncons :: Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance consTypeclassCons' :: (PosCons label func tail row, NegCons label func head row) => Cons label func head tail row where
  cons v (Typeclass h) (Typeclass t) = Typeclass (h <> (pure $ (unsafeCoerce v)) <> t)
  uncons a b=
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

using :: forall x f f' head tail row. Newtype (f x) f' => Cons x f head tail (TypeclassC' f row) => Typeclass (TypeclassC' f row) -> f'
using row = unwrap $ fst (uncons (Proxy :: Proxy x) row)

using_ :: forall x f f' head tail row. Newtype (f x) f' => Cons x f head tail (TypeclassC' f row) => (Unit -> Typeclass (TypeclassC' f row) )-> f'
using_ row = unwrap $ fst (uncons (Proxy :: Proxy x) (row unit))

conz ::
  forall label func tail row.
  Cons label func (TypeclassC' func TypeclassNil') tail row =>
  func label -> Typeclass tail -> Typeclass row
conz a b = cons a tnil b

infixr 5 conz as @>
infixr 5 uncons as @-
infixr 5 union as <@@>
