module Main
  ( main
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (hush)
import Data.Foldable as Foldable
import Data.Maybe (Maybe, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import FS as FS
import Foreign (Foreign)
import Node.ChildProcess as ChildProcess
import Pathy as Pathy
import Prelude (Unit, bind, discard, join, map, pure, unit, void)
import Process as Process
import Simple.JSON as SimpleJSON

type PackageJson =
  { name :: String
  , description :: String
  , version :: String
  , author :: Maybe Foreign
  , bugs :: { url :: String }
  , devDependencies :: Foreign
  , homepage :: String
  , keywords :: Array String
  , license :: String
  , main :: String
  , repository :: { type :: String, url :: String }
  , scripts :: Foreign
  }

type Files =
  { gitIgnore :: Pathy.AbsFile
  , gitIgnoreTemplate :: Pathy.AbsFile
  , license :: Pathy.AbsFile
  , licenseTemplate :: Pathy.AbsFile
  , packageJson :: Pathy.AbsFile
  , readme :: Pathy.AbsFile
  , travisYml :: Pathy.AbsFile
  , travisYmlTemplate :: Pathy.AbsFile
  }

type Dirs =
  { current :: Pathy.AbsDir
  , script :: Pathy.AbsDir
  , templates :: Pathy.AbsDir
  }

addAuthorToReadme :: Files -> Aff Unit
addAuthorToReadme files = do
  Console.log "add 'Author' to README.md"
  let
    text =
      Foldable.intercalate
        "\n"
        [ "## Author"
        , ""
        , "[bouzuya][user] &lt;[m@bouzuya.net][email]&gt; ([https://bouzuya.net/][url])"
        , ""
        , "[user]: https://github.com/bouzuya"
        , "[email]: mailto:m@bouzuya.net"
        , "[url]: https://bouzuya.net/"
        , ""
        ]
  FS.appendTextFile text files.readme

addGitIgnore :: Files -> Aff Unit
addGitIgnore files = do
  Console.log "add .gitignore"
  FS.copyTextFile files.gitIgnoreTemplate files.gitIgnore

addHowToBuildToReadme :: Files -> Aff Unit
addHowToBuildToReadme files = do
  Console.log "add 'How to Build' to README.md"
  let
    text =
      Foldable.intercalate
        "\n"
        [ "## How to build"
        , ""
        , "```bash"
        , "npm install"
        , "```"
        , ""
        ]
  FS.appendTextFile text files.readme

addLicense :: Files -> Aff Unit
addLicense files = do
  Console.log "add LICENSE"
  FS.copyTextFile files.licenseTemplate files.license

addLicenseToReadme :: Files -> Aff Unit
addLicenseToReadme files = do
  Console.log "add 'License' to README.md"
  let
    text =
      Foldable.intercalate
        "\n"
        [ "## License"
        , ""
        , "[MIT](LICENSE)"
        , ""
        ]
  FS.appendTextFile text files.readme

addTravisYml :: Files -> Aff Unit
addTravisYml files = do
  Console.log "add .travis.yml"
  FS.copyTextFile files.travisYmlTemplate files.travisYml

exec :: String -> Array String -> Aff Unit
exec file args =
  void
    (liftEffect
      (ChildProcess.execFileSync file args ChildProcess.defaultExecSyncOptions))

getFiles :: Dirs -> Files
getFiles dirs =
  { gitIgnore:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ ".gitignore")
  , gitIgnoreTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "_gitignore")
  , license: dirs.current Pathy.</> Pathy.file (SProxy :: _ "LICENSE")
  , licenseTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "LICENSE")
  , packageJson:
      dirs.current Pathy.</> Pathy.file (SProxy :: _ "package.json")
  , readme: dirs.current Pathy.</> Pathy.file (SProxy :: _ "README.md")
  , travisYml:
      dirs.current Pathy.</> Pathy.file (SProxy :: _ ".travis.yml")
  , travisYmlTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "_travis.yml")
  }

getDirs :: Effect Dirs
getDirs = do
  current <- Process.currentWorkingDir
  script <- pure Process.scriptDir
  templates <- pure (script Pathy.</> Pathy.dir (SProxy :: _ "templates"))
  pure { current, script, templates }

initPackageJson :: Files -> Aff Unit
initPackageJson files = do
  Console.log "initialize package.json"
  exec "npm" ["init", "--yes"]
  exec "npm" ["install", "--save-dev", "npm-run-all", "purescript", "spago"]
  packageJsonText <- FS.readTextFile files.packageJson
  packageJsonRecord <-
    liftEffect
      (maybe
        (throw "invalid package.json")
        pure
        (SimpleJSON.readJSON_ packageJsonText :: Maybe PackageJson))
  let
    jsonText =
      SimpleJSON.writeJSON
        (packageJsonRecord
          {
            author = do
              authorForeign <- packageJsonRecord.author
              authorString <- SimpleJSON.read_ authorForeign :: Maybe String
              authorRecord <- toAuthorRecord authorString
              pure (SimpleJSON.write authorRecord)
          , scripts =
              SimpleJSON.write
              { build: "spago build"
              , bundle: "spago make-module"
              , docs: "spago sources | xargs purs docs --format html 'src/**/*.purs'"
              , prepare: "npm-run-all -s build bundle"
              , purs: "purs"
              , repl: "spago repl"
              , spago: "spago"
              , test: "spago test"
              }
          })
  FS.writeTextFile files.packageJson jsonText

initSpagoDhall :: Aff Unit
initSpagoDhall = do
  Console.log "initialize spago.dhall"
  exec "npm" ["run", "spago", "--", "init"]
  exec "npm" ["run", "spago", "--", "install", "psci-support", "test-unit"]
  pure unit

toAuthorRecord :: String -> Maybe { email :: String, name :: String, url :: String }
toAuthorRecord s = do
  regex <-
    hush (Regex.regex "^(.+)\\s+<(.+?)>\\s+\\((.+?)\\)$" RegexFlags.noFlags)
  matches <- map NonEmptyArray.toArray (Regex.match regex s)
  name <- join (Array.index matches 1)
  email <- join (Array.index matches 2)
  url <- join (Array.index matches 3)
  pure { email, name, url }

main :: Effect Unit
main = Aff.launchAff_ do
  dirs <- liftEffect getDirs
  files <- pure (getFiles dirs)
  addHowToBuildToReadme files
  addLicense files
  addLicenseToReadme files
  addAuthorToReadme files
  initPackageJson files
  initSpagoDhall
  addGitIgnore files
  addTravisYml files
