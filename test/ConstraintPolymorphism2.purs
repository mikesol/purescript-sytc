module ConstraintPolymorphism2 where

import Prelude
import Control.Lazy (fix)
import Data.Newtype (class Newtype)
import Data.Typeclass (using, Typeclass, type (@@), (<@@>), type (@>), TNil, (@>), tnil)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Showable :: forall k. k -> Type
type Showable t
  = Typeclass (ShowMe @@ (t @> TNil))

showable :: forall t. (t -> String) -> Showable t
showable x = (ShowMe x) @> tnil

intShow :: Showable Int
intShow = showable $ fix \f i ->
  if i > 0 then "1 + " <> f (i - 1)
  else if i < 0 then "-1 + " <> f (i + 1)
  else "0"

boolShow :: Showable Boolean
boolShow = showable $ if _ then "true" else "false"

constraintPolymorphism2 :: Effect Unit
constraintPolymorphism2 = do
  log $ using (intShow <@@> boolShow) true
  log $ using (intShow <@@> boolShow) 5
  log $ using intShow (-1)
