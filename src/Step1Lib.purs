module Step1Lib
  ( Typeclass
  , Typeclass'
  , class Cons
  , cons
  , uncons
  , empty
  , TypeclassNil'
  , TypeclassCons'
  , class HomogeneousOp
  , class HomogeneousOp'
  , class NegCons
  , negCons
  , class Union
  , union
  ) where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

data Typeclass'

foreign import data TypeclassCons' :: forall k. k -> (k -> Type) -> Typeclass' -> Typeclass'
foreign import data TypeclassNil' :: Typeclass'

instance semigroupTypeclassNil' :: Semigroup (Typeclass TypeclassNil') where
 append _ _ = Typeclass Nil

instance monoidTypeclassNil' :: Monoid (Typeclass TypeclassNil') where
 mempty = Typeclass Nil

empty :: Typeclass TypeclassNil'
empty = mempty

newtype Typeclass (c :: Typeclass') = Typeclass (List (Void /\ Void))

class Lacks :: forall (label :: Type). label -> Typeclass' -> Constraint
class Lacks label row 

instance lacksTypeclassNil' :: Lacks a TypeclassNil'
instance lacksTypeclassCons'Fail :: Fail (Text "Typeclass' does not lack this type") => Lacks a (TypeclassCons' a f y)
else instance lacksTypeclassCons' :: Lacks a y => Lacks a (TypeclassCons' b f y)

class HomogeneousOp' :: (Type -> Type) -> Typeclass' -> Constraint
class HomogeneousOp' op row

instance homogeneousOp'Nil :: HomogeneousOp' a TypeclassNil'
else instance homogeneousOp'Cons :: HomogeneousOp' f y => HomogeneousOp' f (TypeclassCons' a f y)

class HomogeneousOp :: Typeclass' -> Constraint
class HomogeneousOp row

instance homogeneousOpNil :: HomogeneousOp TypeclassNil'
else instance homogeneousOpCons :: HomogeneousOp' f y => HomogeneousOp (TypeclassCons' a f y)

class NegCons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class NegCons label func head tail row | label row head -> func tail, label row tail -> func head where
  negCons :: Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance negConsCacheHit :: NegCons l f TypeclassNil' c (TypeclassCons' l f c) where
  negCons _ (Typeclass ((_ /\ a) : b)) = unsafeCoerce a /\ Typeclass Nil /\ Typeclass b
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheMiss :: NegCons l f notC x c => NegCons l f (TypeclassCons' notL f notC) x (TypeclassCons' notL f c) where
  negCons l (Typeclass (a : b)) = let x /\ (Typeclass y) /\ rest = (negCons :: Proxy l -> Typeclass c -> f l /\ Typeclass notC /\ Typeclass x) l (Typeclass b) in x /\ (Typeclass (a : y)) /\ rest
  negCons _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class Cons :: forall (l :: Type). l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Cons label func head tail row | label func head tail -> row, label row -> func head tail where
  cons :: Proxy label -> func label -> Typeclass head -> Typeclass tail -> Typeclass row
  uncons :: Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance consTypeclassCons' :: (HomogeneousOp row, NegCons label func head tail row) => Cons label func head tail row where
  cons k v (Typeclass h) (Typeclass t) = Typeclass (h <> (pure $ (unsafeCoerce k) /\ (unsafeCoerce v)) <> t)
  uncons = negCons

class Union :: Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Union l r row | l r -> row where
  union :: Typeclass l -> Typeclass r -> Typeclass row

instance unionTypeclassNil' :: HomogeneousOp x => Union TypeclassNil' x x where
  union _ = identity

instance unionTypeclassCons' :: (HomogeneousOp a, HomogeneousOp b, HomogeneousOp o,  Union a b o) => Union (TypeclassCons' k v a) b (TypeclassCons' k v o) where
  union (Typeclass a) (Typeclass b) = Typeclass (a <> b)

