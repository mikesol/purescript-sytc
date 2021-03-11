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

showInt :: Int -> String
showInt i
  | i > 0 = "1 + " <> showInt (i-1)
  | i < 0 = "-1 + " <> showInt (i+1)
  | otherwise = "0"

intShow :: Showable Int
intShow = (ShowMe $ showInt) @> tnil

boolShow :: Showable Boolean
boolShow = (ShowMe $ (if _ then "true" else "false")) @> tnil

constraintPolymorphism2 :: Effect Unit
constraintPolymorphism2 = do
  log $ using (intShow <@@> boolShow) true
  log $ using (intShow <@@> boolShow) 5
  log $ using intShow (-1)
