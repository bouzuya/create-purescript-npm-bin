module Test.Person where

import Data.Maybe (Maybe(..))
import Person as Person
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Person" do
  test "fromString" do
    let f = Person.fromString
    Assert.equal
      (Just
        { email: Just "m@bouzuya.net"
        , name: "bouzuya"
        , url: Just "https://bouzuya.net/"
        })
      (f "bouzuya <m@bouzuya.net> (https://bouzuya.net/)")
    Assert.equal
      (Just
        { email: Just "m@bouzuya.net"
        , name: "bouzuya"
        , url: Nothing
        })
      (f "bouzuya <m@bouzuya.net>")
    Assert.equal
      (Just
        { email: Nothing
        , name: "bouzuya"
        , url: Nothing
        })
      (f "bouzuya")
