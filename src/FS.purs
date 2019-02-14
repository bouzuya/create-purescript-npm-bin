module FS
  ( appendTextFile
  , chmod
  , copyTextFile
  , cp
  , mkdir
  , readTextFile
  , writeTextFile
  , module FsPerms
  ) where

import Effect.Aff (Aff)
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Node.FS.Perms (Perm, Perms, all, execute, mkPerms, none, permsFromString, permsToInt, permsToString, read, write) as FsPerms
import Pathy as Pathy
import Prelude (Unit, (<<<), (>>=))

appendTextFile :: String -> Pathy.AbsFile -> Aff Unit
appendTextFile text file =
  Fs.appendTextFile Encoding.UTF8 (pathToString file) text

chmod :: Pathy.AbsFile -> FsPerms.Perms -> Aff Unit
chmod file perms = do
  Fs.chmod (pathToString file) perms

copyTextFile :: Pathy.AbsFile -> Pathy.AbsFile -> Aff Unit
copyTextFile = cp

cp :: Pathy.AbsFile -> Pathy.AbsFile -> Aff Unit
cp src dst = Fs.readFile (pathToString src) >>= Fs.writeFile (pathToString dst)

mkdir :: Pathy.AbsDir -> Aff Unit
mkdir dir = Fs.mkdir (pathToString dir)

pathToString ::
  forall a. Pathy.IsDirOrFile a => Pathy.Path Pathy.Abs a -> String
pathToString = Pathy.printPath Pathy.posixPrinter <<< Pathy.sandboxAny

readTextFile :: Pathy.AbsFile -> Aff String
readTextFile file = Fs.readTextFile Encoding.UTF8 (pathToString file)

writeTextFile :: Pathy.AbsFile -> String -> Aff Unit
writeTextFile file text = Fs.writeTextFile Encoding.UTF8 (pathToString file) text
