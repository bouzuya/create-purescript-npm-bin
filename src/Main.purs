module Main where

import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.Globals (__dirname)
import Node.Path as Path
import Prelude (Unit, bind, discard)
import Snail ((</>))
import Snail as Snail

templateDir :: Snail.Folder
templateDir =
  Snail.folder (Path.concat [__dirname, "templates"])

addAuthor :: Aff Unit
addAuthor = do
  _ <- Snail.echo "add 'Author' to README.md"
  let
    readme = Snail.file "README.md"
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
  Snail.appendFile text readme

addLicense :: Aff Unit
addLicense = do
  _ <- Snail.echo "add LICENSE"
  let
    license = Snail.file "LICENSE"
    src = templateDir </> license
    dst = Snail.folder "."
  Snail.cp src dst Nothing

addLicenseToReadme :: Aff Unit
addLicenseToReadme = do
  _ <- Snail.echo "add 'License' to README.md"
  let
    readme = Snail.file "README.md"
    text =
      Foldable.intercalate
        "\n"
        [ "## License"
        , ""
        , "[MIT](LICENSE)"
        , ""
        ]
  Snail.appendFile text readme

addHowToBuildToReadme :: Aff Unit
addHowToBuildToReadme = do
  _ <- Snail.echo "add 'How to Build' to README.md"
  let
    readme = Snail.file "README.md"
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
  Snail.appendFile text readme

main :: Effect Unit
main = Aff.launchAff_ do
  addHowToBuildToReadme
  addLicense
  addLicenseToReadme
  addAuthor
