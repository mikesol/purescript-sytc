module ExistentialQualification where

import Prelude
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using', (@>))
import Effect (Effect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

type Show'
  = (Function (forall i. i)) @@ String @> TNil

myShow :: Typeclass Show'
myShow = (const "I have no clue what my input is.") @> tnil

existentialQualification :: Effect Unit
existentialQualification = do
  log $ using' myShow (unsafeCoerce false)
