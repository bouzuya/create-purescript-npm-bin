module Test.PackageJsonPerson where

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PackageJsonPerson as Person
import Prelude (discard, map)
import Simple.JSON as SimpleJSON
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "PackageJsonPerson" do
  let
    cases =
      [ Tuple
          { email: Just "m@bouzuya.net"
          , name: "bouzuya"
          , url: Just "https://bouzuya.net/"
          }
          "bouzuya <m@bouzuya.net> (https://bouzuya.net/)"
      , Tuple
          { email: Just "m@bouzuya.net"
          , name: "bouzuya"
          , url: Nothing
          }
          "bouzuya <m@bouzuya.net>"
      , Tuple
          { email: Nothing
          , name: "bouzuya"
          , url: Nothing
          }
          "bouzuya"
      ]
  test "ReadForeign / WriteForeign" do
    for_ cases \(Tuple r s) -> do
      let p = Person.fromString s
      Assert.equal p (SimpleJSON.read_ (SimpleJSON.write s))
      Assert.equal p (SimpleJSON.read_ (SimpleJSON.write r))
      Assert.equal p (SimpleJSON.read_ (SimpleJSON.write p))
  test "fromString / toString" do
    for_ cases \(Tuple r s) -> do
      Assert.equal (Just s) (map Person.toString (Person.fromString s))
