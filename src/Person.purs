module Person
  ( Person
  , fromString
  , toString
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Maybe (Maybe, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Prelude (bind, map, pure, (<>))

type Person =
  { name :: String
  , email :: Maybe String
  , url :: Maybe String
  }

fromString :: String -> Maybe Person
fromString s = do
  regex <-
    Either.hush
      (Regex.regex
        "^(.+?)(?:\\s+<(.+?)>)?(?:\\s+\\((.+?)\\))?$"
        RegexFlags.noFlags)
  matches <- map NonEmptyArray.toArray (Regex.match regex s)
  nameMaybe <- Array.index matches 1
  name <- nameMaybe -- name is required
  emailMaybe <- Array.index matches 2
  urlMaybe <- Array.index matches 3
  pure { email: emailMaybe, name, url: urlMaybe }

toString :: Person -> String
toString { email, name, url } =
  name <> (f "<" email ">") <> (f "(" url ")")
  where
    f o m c = maybe "" (\v -> " " <> o <> v <> c) m
