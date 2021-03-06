module Main
  ( main
  ) where

import Data.Either as Either
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
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
import Options (Options)
import Options as Options
import PackageJsonPerson (PackageJsonPerson)
import Pathy as Pathy
import Prelude (Unit, bind, discard, pure, unit, void, (+), (<<<), (<>))
import Process as Process
import Simple.JSON as SimpleJSON

type PackageJson =
  { author :: Maybe PackageJsonPerson
  , bin :: Maybe String
  , bugs :: { url :: String }
  , description :: String
  , devDependencies :: Foreign
  , files :: Maybe (Array String)
  , homepage :: String
  , keywords :: Array String
  , license :: String
  , main :: String
  , name :: String
  , repository :: { type :: String, url :: String }
  , scripts :: Foreign
  , version :: String
  }

type Files =
  { bin :: Pathy.AbsFile
  , gitIgnore :: Pathy.AbsFile
  , gitIgnoreTemplate :: Pathy.AbsFile
  , license :: Pathy.AbsFile
  , licenseTemplate :: Pathy.AbsFile
  , packageJson :: Pathy.AbsFile
  , readme :: Pathy.AbsFile
  , travisYml :: Pathy.AbsFile
  , travisYmlTemplate :: Pathy.AbsFile
  }

type Dirs =
  { bin :: Pathy.AbsDir
  , current :: Pathy.AbsDir
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
        [ ""
        , "## Author"
        , ""
        , "[bouzuya][user] &lt;[m@bouzuya.net][email]&gt; ([https://bouzuya.net/][url])"
        , ""
        , "[user]: https://github.com/bouzuya"
        , "[email]: mailto:m@bouzuya.net"
        , "[url]: https://bouzuya.net/"
        , ""
        ]
  FS.appendTextFile text files.readme

addBin :: Dirs -> Files -> Aff Unit
addBin dirs files = do
  Console.log "add bin/..."
  let
    text =
      Foldable.intercalate
        "\n"
        [ "#!/usr/bin/env node"
        , "require('../').main();"
        ]
  FS.mkdir dirs.bin
  FS.writeTextFile files.bin text
  -- 0755
  FS.chmod
    files.bin
    (FS.mkPerms
      FS.all
      (FS.read + FS.execute)
      (FS.read + FS.execute))

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
        [ ""
        , "## How to build"
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
        [ ""
        , "## License"
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

getFiles :: Dirs -> String -> Files
getFiles dirs name =
  { bin:
      dirs.bin Pathy.</>
        (Maybe.fromMaybe
          (Pathy.file (SProxy :: _ "dummy"))
          (Pathy.parseRelFile Pathy.posixParser name))
  , gitIgnore: dirs.current Pathy.</> Pathy.file (SProxy :: _ ".gitignore")
  , gitIgnoreTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "_gitignore")
  , license: dirs.current Pathy.</> Pathy.file (SProxy :: _ "LICENSE")
  , licenseTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "LICENSE")
  , packageJson:
      dirs.current Pathy.</> Pathy.file (SProxy :: _ "package.json")
  , readme: dirs.current Pathy.</> Pathy.file (SProxy :: _ "README.md")
  , travisYml: dirs.current Pathy.</> Pathy.file (SProxy :: _ ".travis.yml")
  , travisYmlTemplate:
      dirs.templates Pathy.</> Pathy.file (SProxy :: _ "_travis.yml")
  }

getDirs :: Effect Dirs
getDirs = do
  current <- Process.currentWorkingDir
  bin <- pure (current Pathy.</> Pathy.dir (SProxy :: _ "bin"))
  script <- pure Process.scriptDir
  templates <- pure (script Pathy.</> Pathy.dir (SProxy :: _ "templates"))
  pure { bin, current, script, templates }

initPackageJson :: Files -> String -> Aff Unit
initPackageJson files name = do
  Console.log "initialize package.json"
  exec "npm" ["init", "--yes"]
  exec "npm" ["install", "--save-dev", "purescript", "spago"]
  packageJsonText <- FS.readTextFile files.packageJson
  packageJsonRecord <-
    liftEffect
      (Maybe.maybe
        (throw "invalid package.json")
        pure
        (SimpleJSON.readJSON_ packageJsonText :: Maybe PackageJson))
  let
    jsonText =
      SimpleJSON.writeJSON
        (packageJsonRecord
          { bin = Just ("bin/" <> name)
          , files = Just ["bin"]
          , scripts =
              SimpleJSON.write
              { build: "spago make-module"
              , docs: "spago docs"
              , prepare: "npm run build"
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
  let
    dependencies =
      [ "node-process"
      , "psci-support"
      , "test-unit"
      ]
  exec "npm" (["run", "spago", "--", "install"] <> dependencies)
  pure unit

main :: Effect Unit
main = Aff.launchAff_ do
  args <- liftEffect Process.args
  options <- Either.either (liftEffect <<< throw) pure (Options.parse args)
  if options.help
    then liftEffect showUsage
    else init options

init :: Options -> Aff Unit
init options = do
  dirs <- liftEffect getDirs
  files <- pure (getFiles dirs options.name)
  addHowToBuildToReadme files
  addLicense files
  addLicenseToReadme files
  addAuthorToReadme files
  addBin dirs files
  initPackageJson files options.name
  initSpagoDhall
  addGitIgnore files
  addTravisYml files

showUsage :: Effect Unit
showUsage = Console.log Options.usage
