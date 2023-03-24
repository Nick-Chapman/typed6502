
module Top (main) where

import System.Environment (getArgs)
import qualified UntypedAsm as Asm (writeBytes)

import qualified CharX
import qualified Hello
import qualified Goodbye

main :: IO ()
main = do
  [filename] <- getArgs
  Asm.writeBytes filename (codeFor filename)
  where
    codeFor = \case
      "_build/charx-haskell.bytes" -> CharX.code
      "_build/hello-haskell.bytes" -> Hello.code
      "_build/goodbye-haskell.bytes" -> Goodbye.code
      s ->
        error (show ("unknown output file",s))
