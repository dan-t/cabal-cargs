
module CabalCargs.Format
   ( format
   ) where

import CabalCargs.CompilerArgs (CompilerArgs(..))
import CabalCargs.Formatting (Formatting(..))


format :: Formatting -> CompilerArgs -> [String]
format fmt cargs = [""]
