module Process
  ( args
  , currentWorkingDir
  , scriptDir
  ) where

import Data.Array as Array
import Data.Maybe (fromJust, maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Globals (__dirname)
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Pathy as Pathy
import Prelude (bind, map, pure, (<>))

args :: Effect (Array String)
args = map (Array.drop 2) Process.argv

currentWorkingDir :: Effect Pathy.AbsDir
currentWorkingDir = do
  cwd <- Process.cwd
  let dirMaybe = Pathy.parseAbsDir Pathy.posixParser (cwd <> "/")
  maybe (throw "invalid cwd") pure dirMaybe

scriptDir :: Pathy.AbsDir
scriptDir =
  -- "/**/" is AbsDir
  let dirMaybe = Pathy.parseAbsDir Pathy.posixParser (__dirname <> "/")
  in unsafePartial (fromJust dirMaybe)
