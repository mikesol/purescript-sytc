module Test.Main where

import Prelude
import Basic (basic)
import ConstraintPolymorphism (constraintPolymorphism)
import Effect (Effect)
import Effect.Class.Console (log)
import ExistentialQualification1 (existentialQualification1)
import ExistentialQualification2 (existentialQualification2)
import Recursive1 (recursive1)
import Recursive2 (recursive2)

--import ShowOverride (showOverride)
main :: Effect Unit
main = do
  log "**** basic"
  basic
  log "**** recursive1"
  recursive1
  log "**** recursive2"
  recursive2
  log "**** constraint polymorphism"
  constraintPolymorphism
  log "**** existential qualification 1"
  existentialQualification1
  log "**** existential qualification 2"
  existentialQualification2
