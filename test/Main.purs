module Test.Main where

import Prelude
import Basic (basic)
import ConstraintPolymorphism (constraintPolymorphism)
import ConstraintPolymorphism2 (constraintPolymorphism2)
import Effect (Effect)
import Effect.Class.Console (log)
import ExistentialQualification1 (existentialQualification1)
import ExistentialQualification2 (existentialQualification2)
import Polymorphism (polymorphism)
import Recursive1 (recursive1)
import Recursive2 (recursive2)
import Test.Functor (functor)

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
  log "**** constraint polymorphism 2"
  constraintPolymorphism2
  log "**** existential qualification 1"
  existentialQualification1
  log "**** existential qualification 2"
  existentialQualification2
  log "**** functor"
  functor
  log "**** polymorphism"
  polymorphism
