module PackageJsonPerson
  ( PackageJsonPerson
  , fromString
  , toString
  ) where

import Control.Alt ((<|>))
import Data.Maybe (Maybe, maybe)
import Foreign (ForeignError(..), fail)
import Person (Person)
import Person as Person
import Prelude (class Eq, class Show, map, pure, show, (<<<), (<>), (>>=))
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Simple.JSON as SimpleJSON

newtype PackageJsonPerson = PackageJsonPerson Person

derive instance eqPackageJsonPerson :: Eq PackageJsonPerson

instance readForeignPackageJsonPerson :: ReadForeign PackageJsonPerson where
  readImpl f =
    (SimpleJSON.readImpl f >>=
      ((maybe (fail (ForeignError "invalid person")) pure) <<< fromString)) <|>
    (SimpleJSON.readImpl f >>=
      (pure <<< PackageJsonPerson))

instance showPackageJsonPerson :: Show PackageJsonPerson where
  show (PackageJsonPerson r) = "(PackageJsonPerson " <> show r <> ")"

instance writeForeignPackageJsonPerson :: WriteForeign PackageJsonPerson where
  writeImpl (PackageJsonPerson r) = writeImpl r

fromString :: String -> Maybe PackageJsonPerson
fromString s = map PackageJsonPerson (Person.fromString s)

toString :: PackageJsonPerson -> String
toString (PackageJsonPerson r) = Person.toString r
