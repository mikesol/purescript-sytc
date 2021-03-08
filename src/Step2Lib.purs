module Step2Lib
  ( Typeclass
  , Typeclass'
  , class Cons
  , cons
  , uncons
  , empty
  , TypePair'
  , TypePairC'
  , TypeclassNil'
  , TypeclassCons'
  , class Unpair
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

data TypePair'

foreign import data TypePairC' :: forall k. k -> (k -> Type) -> TypePair'

data Typeclass'

foreign import data TypeclassCons' :: TypePair' -> Typeclass' -> Typeclass'
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
instance lacksTypeclassCons'Fail :: Fail (Text "Typeclass' does not lack this type") => Lacks a (TypeclassCons' (TypePairC' a f) y)
else instance lacksTypeclassCons' :: Lacks a y => Lacks a (TypeclassCons' (TypePairC' b f) y)

class HomogeneousOp' :: (Type -> Type) -> Typeclass' -> Constraint
class HomogeneousOp' op row

instance homogeneousOp'Nil :: HomogeneousOp' a TypeclassNil'
else instance homogeneousOp'Cons :: HomogeneousOp' f y => HomogeneousOp' f (TypeclassCons' (TypePairC' a f) y)

class HomogeneousOp :: Typeclass' -> Constraint
class HomogeneousOp row

instance homogeneousOpNil :: HomogeneousOp TypeclassNil'
else instance homogeneousOpCons :: HomogeneousOp' f y => HomogeneousOp (TypeclassCons' (TypePairC' a f) y)

class Unpair :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class Unpair k v pair | k v -> pair, pair -> k v

instance unpairUnpair :: Unpair k v (TypePairC' k v)

class NegCons :: forall k. TypePair' -> k -> (k -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Unpair label func pair <= NegCons pair label func head tail row | label row head -> func tail, label row tail -> func head where
  negCons :: Proxy pair -> Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance negConsCacheHit :: Unpair l f (TypePairC' l f) => NegCons (TypePairC' l f) l f TypeclassNil' c (TypeclassCons' (TypePairC' l f) c) where
  negCons _ _ (Typeclass ((_ /\ a) : b)) = unsafeCoerce a /\ Typeclass Nil /\ Typeclass b
  negCons _ _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"
else instance negConsCacheMiss :: (Unpair notL f (TypePairC' notL f), Unpair l f (TypePairC' l f), NegCons (TypePairC' l f) l f notC x c) => NegCons (TypePairC' l f ) l f (TypeclassCons' (TypePairC' notL f) notC) x (TypeclassCons' (TypePairC' notL f) c) where
  negCons i l (Typeclass (a : b)) = let x /\ (Typeclass y) /\ rest = (negCons :: Proxy (TypePairC' l f) -> Proxy l -> Typeclass c -> f l /\ Typeclass notC /\ Typeclass x) i l (Typeclass b) in x /\ (Typeclass (a : y)) /\ rest
  negCons _ _ (Typeclass Nil) = unsafeCrashWith "you shouldn't be here"

class Cons :: forall (l :: Type). TypePair' -> l -> (l -> Type) -> Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Cons pair label func head tail row | label func head tail -> row, label row -> func head tail where
  cons :: Proxy pair -> Proxy label -> func label -> Typeclass head -> Typeclass tail -> Typeclass row
  uncons :: Proxy pair -> Proxy label -> Typeclass row -> func label /\ Typeclass head /\ Typeclass tail

instance consTypeclassCons' :: (Unpair label func pair, HomogeneousOp row, NegCons pair label func head tail row) => Cons pair label func head tail row where
  cons _ k v (Typeclass h) (Typeclass t) = Typeclass (h <> (pure $ (unsafeCoerce k) /\ (unsafeCoerce v)) <> t)
  uncons = negCons

class Union :: Typeclass' -> Typeclass' -> Typeclass' -> Constraint
class Union l r row | l r -> row where
  union :: Typeclass l -> Typeclass r -> Typeclass row

instance unionTypeclassNil' :: HomogeneousOp x => Union TypeclassNil' x x where
  union _ = identity

instance unionTypeclassCons' :: (HomogeneousOp a, HomogeneousOp b, HomogeneousOp o,  Union a b o) => Union (TypeclassCons' (TypePairC' k v) a) b (TypeclassCons' (TypePairC' k v) o) where
  union (Typeclass a) (Typeclass b) = Typeclass (a <> b)

