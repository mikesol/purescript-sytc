module Test.Main where

import Prelude
import Basic (basic)
import ConstraintPolymorphism (constraintPolymorphism)
import Effect (Effect)
import Effect.Class.Console (log)
import Recursive1 (recursive1)
import Recursive2 (recursive2)
import ShowOverride (showOverride)

main :: Effect Unit
main = do
  log "**** basic"
  basic
  log "**** showOverride"
  showOverride
  log "**** recursive1"
  recursive1
  log "**** recursive2"
  recursive2
  log "**** constraint polymorphism"
  constraintPolymorphism
