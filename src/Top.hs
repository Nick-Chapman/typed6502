
module Top (main) where

import Data.Word (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.ByteString as ByteString (pack,writeFile)

import qualified CharX
import qualified Hello
import qualified Goodbye
import qualified Greetings
import qualified Frame
import qualified Bf

main :: IO ()
main = do
  args <- getArgs
  let Config{name,output} = parse args
  writeBytes output (example name)

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
  "Frame" -> Frame.code
  "Bf" -> Bf.code
  name ->
    error $ printf "unexpected example: %s\n" name

writeBytes :: FilePath -> [Word8] -> IO ()
writeBytes path bs = do
  printf "writeBytes (#%d) --> %s\n" (length bs) path
  ByteString.writeFile path (ByteString.pack bs)
