module Main where

import Prelude
import Effect (Effect)
import Step1111Impl (step1111)
import Step111Impl (step111)
import Step1Impl (step1)
import Step2Impl (step2)

main :: Effect Unit
main = step1111
