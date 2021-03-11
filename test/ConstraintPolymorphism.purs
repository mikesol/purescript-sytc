module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (using, Typeclass, type (@@), type (@>), TNil, (@>), tnil)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Showable t a
  = Typeclass (ShowMe @@ (t @> a))

intShow :: forall a. Typeclass (ShowMe @@ a) -> Showable Int a
intShow a = (ShowMe $ (show :: Int -> String)) @> a

boolShow :: forall a. Typeclass (ShowMe @@ a) -> Showable Boolean a
boolShow a = (ShowMe $ (show :: Boolean -> String)) @> a

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ using (intShow (boolShow tnil)) true
  log $ using (intShow tnil) 1
