
module Top (main) where

import Data.Word (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified UntypedAsm as Asm (writeBytes)

import qualified CharX
import qualified Goodbye
import qualified Hello

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
  "charx" -> CharX.code
  "hello" -> Hello.code
  "goodbye" -> Goodbye.code
  name ->
    error $ printf "unexpected example: %s\n" name
