
module Top (main) where

import System.Environment (getArgs)
import qualified UntypedAsm as Asm (writeBytes)

import qualified CharX (code)
import qualified Hello (code)

main :: IO ()
main = do
  [filename] <- getArgs
  Asm.writeBytes filename (codeFor filename)
  where
    codeFor = \case
      "_build/charx.haskell-bytes" -> CharX.code
      "_build/hello.haskell-bytes" -> Hello.code
      s ->
        error (show ("unknown output file",s))
