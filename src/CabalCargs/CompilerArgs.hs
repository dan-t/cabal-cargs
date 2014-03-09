
module CabalCargs.CompilerArgs
   ( CompilerArgs(..)
   , compilerArgs
   ) where

import CabalCargs.CargsSpec (CargsSpec)
import System.Console.CmdArgs.Default (Default, def)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.List (nub)
import Control.Applicative ((<|>))

-- | The collected compiler args from the cabal file.
data CompilerArgs = CompilerArgs 
   { hsSourceDirs   :: [FilePath]
   , ghcOptions     :: [String]
   , extensions     :: [String]
   , cppOptions     :: [String]
   , cSources       :: [FilePath]
   , ccOptions      :: [String]
   , extraLibDirs   :: [FilePath]
   , extraLibraries :: [FilePath]
   , ldOptions      :: [String]
   , includeDirs    :: [FilePath]
   , includes       :: [FilePath]
   , packageDB      :: Maybe FilePath
   }
   deriving (Show, Eq)


-- | Collect the compiler args specified by 'CargsSpec'.
compilerArgs :: CargsSpec -> CompilerArgs
compilerArgs spec = def


instance Default CompilerArgs where
   def = CompilerArgs { hsSourceDirs   = def
                      , ghcOptions     = def
                      , extensions     = def
                      , cppOptions     = def
                      , cSources       = def
                      , ccOptions      = def
                      , extraLibDirs   = def
                      , extraLibraries = def
                      , ldOptions      = def
                      , includeDirs    = def
                      , includes       = def
                      , packageDB      = def
                      }


instance Monoid CompilerArgs where
   mempty          = def
   mappend ca1 ca2 = let cas = [ca1, ca2] in CompilerArgs
      { hsSourceDirs   = nub . mconcat . map hsSourceDirs   $ cas
      , ghcOptions     = nub . mconcat . map ghcOptions     $ cas
      , extensions     = nub . mconcat . map extensions     $ cas
      , cppOptions     = nub . mconcat . map cppOptions     $ cas
      , cSources       = nub . mconcat . map cSources       $ cas
      , ccOptions      = nub . mconcat . map ccOptions      $ cas
      , extraLibDirs   = nub . mconcat . map extraLibDirs   $ cas
      , extraLibraries = nub . mconcat . map extraLibraries $ cas
      , ldOptions      = nub . mconcat . map ldOptions      $ cas
      , includeDirs    = nub . mconcat . map includeDirs    $ cas
      , includes       = nub . mconcat . map includes       $ cas
      , packageDB      = packageDB ca1 <|> packageDB ca2
      }
