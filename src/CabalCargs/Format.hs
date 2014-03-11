{-# Language PatternGuards #-}

module CabalCargs.Format
   ( format
   ) where

import CabalCargs.CompilerArgs (CompilerArgs(..))
import CabalCargs.Formatting (Formatting(..))
import Data.Maybe (maybeToList)
import Data.List (foldl')
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path ((</>))


format :: Formatting -> CompilerArgs -> [String]
format Ghc cargs = concat [ map ("-i" ++) (hsSourceDirs cargs)
                          , ["-i" ++ autogenDir]
                          , ghcOptions cargs
                          , map ("-X" ++) (defaultExtensions cargs)
                          , map ("-optP" ++) (cppOptions cargs)
                          , map ("-optc" ++) (ccOptions cargs)
                          , map ("-L" ++) (extraLibDirs cargs)
                          , map ("-l" ++) (extraLibraries cargs)
                          , map ("-I" ++) (includeDirs cargs)
                          , ["-I" ++ autogenDir]
                          , ["-optP-include", "-optP" ++ autogenDir ++ "/cabal_macros.h"]
                          , ghcIncludes
                          , maybe [""] (\db -> ["-package-conf=" ++ db]) (packageDB cargs)
                          ]

   where
      ghcIncludes =
         reverse $ foldl' addInclude [] (includes cargs)
         where
            addInclude incs inc = ("-optP" ++ inc) : ("-optP-include") : incs

      autogenDir = prependCabalDir cargs "dist/build/autogen"


format Hdevtools cargs = (map ("-g" ++) (format Ghc cargs)) ++ socket
   where
      socket = ["--socket=" ++ prependCabalDir cargs ".hdevtools.sock"]

format Pure cargs = concat [ hsSourceDirs cargs
                           , ghcOptions cargs
                           , defaultExtensions cargs
                           , cppOptions cargs
                           , cSources cargs
                           , ccOptions cargs
                           , extraLibDirs cargs
                           , extraLibraries cargs
                           , ldOptions cargs
                           , includeDirs cargs
                           , includes cargs
                           , maybeToList $ packageDB cargs
                           ]


prependCabalDir :: CompilerArgs -> String -> String
prependCabalDir cargs path = FP.encodeString $ cabalDir </> (FP.decodeString path)
   where
      cabalDir = FP.directory $ FP.decodeString (cabalFile cargs)
