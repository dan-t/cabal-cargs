
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified CabalCargs.Args as CmdArgs
import qualified CabalCargs.Format as F
import qualified CabalCargs.CargsSpec as CargsSpec
import CabalCargs.CompilerArgs (compilerArgs)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (runEitherT)
import Data.List (intercalate)


main :: IO ()
main = do
   cmdArgs <- CmdArgs.get
   (either withError withSpec) =<< runEitherT (CargsSpec.fromCmdArgs cmdArgs)
   where
      withError error = do
         hPutStrLn stderr ("cabal-cargs: " ++ error)
         exitFailure

      withSpec spec = do
         format <- CmdArgs.format <$> CmdArgs.get
         putStr $ intercalate " " $ F.format format (compilerArgs spec)
         exitSuccess
