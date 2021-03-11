module ConstraintPolymorphism2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (using, Typeclass, type (@@), (<@@>), type (@>), TNil, (@>), tnil)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Showable t
  = Typeclass (ShowMe @@ (t @> TNil))

intShow :: Showable Int
intShow = (ShowMe $ (show :: Int -> String)) @> tnil

boolShow :: Showable Boolean
boolShow = (ShowMe $ (show :: Boolean -> String)) @> tnil

constraintPolymorphism2 :: Effect Unit
constraintPolymorphism2 = do
  log $ using (intShow <@@> boolShow) true
  log $ using (intShow <@@> boolShow) 5
  log $ using intShow 1
