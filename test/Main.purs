module Test.Main where

import Prelude

import Effect (Effect)
import Test.Person as Person
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Person.tests
