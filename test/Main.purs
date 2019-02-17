module Test.Main where

import Prelude

import Effect (Effect)
import Test.PackageJsonPerson as PackageJsonPerson
import Test.Person as Person
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  PackageJsonPerson.tests
  Person.tests
