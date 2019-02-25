module Options
  ( Options
  , parse
  , usage
  ) where

import Bouzuya.CommandLineOption as CommandLineOption
import Data.Either (Either)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Prelude (map)

type Options =
  { help :: Boolean
  , name :: String
  }

defs ::
  { help :: CommandLineOption.OptionDefinition Boolean
  , name :: CommandLineOption.OptionDefinition String
  }
defs =
  { help: CommandLineOption.booleanOption "help" Nothing "show help"
  , name:
      CommandLineOption.stringOption
        "name" Nothing "<NAME>" "project (bin) name (`bin/NAME`)" "my-project"
  }

parse :: Array String -> Either String Options
parse args = map _.options (CommandLineOption.parse defs args)

usage :: String
usage =
  Foldable.intercalate
    "\n"
    [ "Usage: purescript-npm-bin [options...]"
    , ""
    , "Options:"
    , ""
    , "  --help        show help"
    , "  --name <NAME> project (bin) name (`bin/NAME`)"
    , ""
    ]
