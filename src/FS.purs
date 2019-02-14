module FS
  ( appendTextFile
  , copyTextFile
  , cp
  , readTextFile
  , writeTextFile
  ) where

import Effect.Aff (Aff)
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Pathy as Pathy
import Prelude (Unit, (<<<), (>>=))

appendTextFile :: String -> Pathy.AbsFile -> Aff Unit
appendTextFile text file =
  Fs.appendTextFile Encoding.UTF8 (pathToString file) text

copyTextFile :: Pathy.AbsFile -> Pathy.AbsFile -> Aff Unit
copyTextFile = cp

cp :: Pathy.AbsFile -> Pathy.AbsFile -> Aff Unit
cp src dst = Fs.readFile (pathToString src) >>= Fs.writeFile (pathToString dst)

pathToString :: Pathy.AbsFile -> String
pathToString = Pathy.printPath Pathy.posixPrinter <<< Pathy.sandboxAny

readTextFile :: Pathy.AbsFile -> Aff String
readTextFile file = Fs.readTextFile Encoding.UTF8 (pathToString file)

writeTextFile :: Pathy.AbsFile -> String -> Aff Unit
writeTextFile file text = Fs.writeTextFile Encoding.UTF8 (pathToString file) text
