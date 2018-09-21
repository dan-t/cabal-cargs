{-# Language PatternGuards #-}

module CabalCargs.Format
   ( format
   ) where

import CabalCargs.CompilerArgs (CompilerArgs(..))
import CabalCargs.Formatting (Formatting(..))
import Data.Maybe (maybeToList)
import Data.List (foldl')


format :: Formatting -> CompilerArgs -> [String]
format Ghc cargs = concat [ formatHsSourceDirs $ hsSourceDirs cargs
                          , ghcOptions cargs
                          , map ("-X" ++) (defaultExtensions cargs)
                          , map ("-X" ++) (defaultLanguage cargs)
                          , map ("-optP" ++) (cppOptions cargs)
                          , map ("-optc" ++) (ccOptions cargs)
                          , map ("-L" ++) (extraLibDirs cargs)
                          , map ("-l" ++) (extraLibraries cargs)
                          , formatIncludeDirs $ includeDirs cargs
                          , formatIncludes $ includes cargs
                          , formatBuildDepends $ buildDepends cargs
                          , maybe []
                                  (\db -> ["-clear-package-db", "-global-package-db", "-package-db=" ++ db])
                                  (packageDB cargs)
                          , formatHsSourceDirs $ autogenHsSourceDirs cargs
                          , formatIncludeDirs $ autogenIncludeDirs cargs
                          , formatIncludes $ autogenIncludes cargs
                          ]
   where
      formatBuildDepends []   = []
      formatBuildDepends deps = map ("-package=" ++) deps

      formatHsSourceDirs = map ("-i" ++)
      formatIncludeDirs  = map ("-I" ++)

      formatIncludes incs = reverse $ foldl' addInclude [] incs
         where
            addInclude incs inc = ("-optP" ++ inc) : ("-optP-include") : incs


format Hdevtools cargs = (map ("-g" ++) (format Ghc cargs)) ++ socket
   where
      socket = maybe [] (\s -> ["--socket=" ++ s]) (hdevtoolsSocket cargs)


format Pure cargs = concat [ hsSourceDirs cargs
                           , ghcOptions cargs
                           , defaultExtensions cargs
                           , defaultLanguage cargs
                           , cppOptions cargs
                           , cSources cargs
                           , ccOptions cargs
                           , extraLibDirs cargs
                           , extraLibraries cargs
                           , ldOptions cargs
                           , includeDirs cargs
                           , includes cargs
                           , buildDepends cargs
                           , maybeToList $ packageDB cargs
                           , maybeToList $ rootDir cargs
                           , autogenHsSourceDirs cargs
                           , autogenIncludeDirs cargs
                           , autogenIncludes cargs
                           , maybeToList $ hdevtoolsSocket cargs
                           ]
