module Main
  ( main
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (hush)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Node.Globals (__dirname)
import Node.Path as Path
import Prelude (Unit, bind, discard, join, map, pure, unit)
import Simple.JSON as SimpleJSON
import Snail ((</>))
import Snail as Snail

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

dirs :: { current :: Snail.Folder, templates :: Snail.Folder }
dirs =
  -- `*/` is workaround for snail pathpend (</>) bug
  { current: Snail.folder "./"
  , templates: Snail.folder (Path.concat [__dirname, "templates/"])
  }

files ::
  { gitIgnore :: Snail.File
  , gitIgnoreTemplate :: Snail.File
  , license :: Snail.File
  , licenseTemplate :: Snail.File
  , readme :: Snail.File
  , travisYml :: Snail.File
  , travisYmlTemplate :: Snail.File
  }
files =
  { gitIgnore: dirs.templates </> Snail.file ".gitignore"
  , gitIgnoreTemplate: dirs.templates </> Snail.file "_gitignore"
  , license: dirs.current </> Snail.file "LICENSE"
  , licenseTemplate: dirs.templates </> Snail.file "LICENSE"
  , readme: dirs.current </> Snail.file "README.md"
  , travisYml: dirs.current </> Snail.file ".travis.yml"
  , travisYmlTemplate: dirs.templates </> Snail.file "_travis.yml"
  }

addAuthorToReadme :: Aff Unit
addAuthorToReadme = do
  _ <- Snail.echo "add 'Author' to README.md"
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
  Snail.appendFile text files.readme

addGitIgnore :: Aff Unit
addGitIgnore = do
  _ <- Snail.echo "add .gitignore"
  Snail.cp files.gitIgnoreTemplate dirs.current Nothing

addHowToBuildToReadme :: Aff Unit
addHowToBuildToReadme = do
  _ <- Snail.echo "add 'How to Build' to README.md"
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
  Snail.appendFile text files.readme

addLicense :: Aff Unit
addLicense = do
  _ <- Snail.echo "add LICENSE"
  Snail.cp files.licenseTemplate dirs.current Nothing

addLicenseToReadme :: Aff Unit
addLicenseToReadme = do
  _ <- Snail.echo "add 'License' to README.md"
  let
    text =
      Foldable.intercalate
        "\n"
        [ "## License"
        , ""
        , "[MIT](LICENSE)"
        , ""
        ]
  Snail.appendFile text files.readme

addTravisYml :: Aff Unit
addTravisYml = do
  _ <- Snail.echo "add .travis.yml"
  Snail.cp files.travisYmlTemplate dirs.current Nothing

initPackageJson :: Aff Unit
initPackageJson = do
  _ <- Snail.echo "initialize package.json"
  _ <- Snail.exec ("npm" :| ["init", "--yes"])
  _ <-
    Snail.exec
      ("npm" :| ["install", "--save-dev", "npm-run-all", "purescript", "spago"])
  packageJsonText <- Fs.readTextFile Encoding.UTF8 "package.json"
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
  Fs.writeTextFile Encoding.UTF8 "package.json" jsonText

initSpagoDhall :: Aff Unit
initSpagoDhall = do
  _ <- Snail.echo "initialize spago.dhall"
  _ <- Snail.exec ("npm" :| ["run", "spago", "--", "init"])
  _ <-
    Snail.exec
      ("npm" :| ["run", "spago", "--", "install", "psci-support", "test-unit"])
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
  addHowToBuildToReadme
  addLicense
  addLicenseToReadme
  addAuthorToReadme
  initPackageJson
  initSpagoDhall
  addGitIgnore
  addTravisYml
