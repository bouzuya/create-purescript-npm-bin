module Process
  ( currentWorkingDir
  , scriptDir
  ) where

import Data.Maybe (fromJust, maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Globals (__dirname)
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Pathy as Pathy
import Prelude (bind, pure)

currentWorkingDir :: Effect Pathy.AbsDir
currentWorkingDir = do
  cwd <- Process.cwd
  maybe (throw "invalid cwd") pure (Pathy.parseAbsDir Pathy.posixParser cwd)

scriptDir :: Pathy.AbsDir
scriptDir =
  let dirMaybe = Pathy.parseAbsDir Pathy.posixParser __dirname
  in unsafePartial (fromJust dirMaybe)
