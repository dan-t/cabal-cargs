
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified CabalCargs.Args as CmdArgs
import qualified CabalCargs.Format as F
import qualified CabalCargs.CompilerArgs as CompilerArgs
import Data.List (intercalate)


main :: IO ()
main = do
   cmdArgs <- CmdArgs.get
   let formatting = CmdArgs.format cmdArgs
   (either withError (withCargs formatting)) =<< (CompilerArgs.fromCmdArgs cmdArgs)
   where
      withError error = do
         hPutStrLn stderr ("cabal-cargs: " ++ error)
         exitFailure

      withCargs formatting cargs = do
         putStr $ intercalate " " $ F.format formatting cargs
         exitSuccess
