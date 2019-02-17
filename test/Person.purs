module Test.Person where

import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Person as Person
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Person" do
  test "fromString / toString" do
    let
      from = Person.fromString
      to = Person.toString
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
    for_ cases \(Tuple r s) -> do
      Assert.equal (Just r) (from s)
      Assert.equal s (to r)
