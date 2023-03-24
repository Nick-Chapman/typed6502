
module Top (main) where

import Data.Word (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified UntypedAsm as Asm (writeBytes)

import qualified CharX
import qualified Hello
import qualified Goodbye
import qualified Greetings

main :: IO ()
main = do
  args <- getArgs
  let Config{name,output} = parse args
  Asm.writeBytes output (example name)

data Config = Config { name :: String, output :: FilePath }

parse :: [String] -> Config
parse = \case
  [name,output] -> Config { name, output }
  args ->
    error $ printf "unexpected command line: %s\n" (show args)

example :: String -> [Word8]
example = \case
  "CharX" -> CharX.code
  "Hello" -> Hello.code
  "Goodbye" -> Goodbye.code
  "Greetings" -> Greetings.code
  name ->
    error $ printf "unexpected example: %s\n" name
