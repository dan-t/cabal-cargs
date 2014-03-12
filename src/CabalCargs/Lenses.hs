{-# LANGUAGE TemplateHaskell, Rank2Types, PatternGuards #-}

module CabalCargs.Lenses
   ( buildInfoOfLib
   , buildInfoOfExe
   , buildInfoOfTest
   , buildInfoOfBenchm
   , buildInfoOf
   , allBuildInfos
   , field
   , defaultLang
   , langToString
   ) where

import Distribution.PackageDescription
import Distribution.Compiler
import Language.Haskell.Extension
import Control.Lens
import Data.Data.Lens (biplate)
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Field as F

makeLensesFor [ ("condLibrary"    , "condLibraryL")
              , ("condExecutables", "condExecutablesL")
              , ("condTestSuites" , "condTestSuitesL")
              , ("condBenchmarks" , "condBenchmarksL")
              ] ''GenericPackageDescription

makeLensesFor [ ("hsSourceDirs"     , "hsSourceDirsL")
              , ("options"          , "optionsL")
              , ("defaultLanguage"  , "defaultLang")
              , ("cppOptions"       , "cppOptionsL")
              , ("cSources"         , "cSourcesL")
              , ("ccOptions"        , "ccOptionsL")
              , ("extraLibDirs"     , "extraLibDirsL")
              , ("extraLibs"        , "extraLibsL")
              , ("ldOptions"        , "ldOptionsL")
              , ("includeDirs"      , "includeDirsL")
              , ("includes"         , "includesL")
              ] ''BuildInfo


buildInfoOfLib :: Traversal' GenericPackageDescription BuildInfo
buildInfoOfLib = condLibraryL . _Just . biplate


buildInfoOfExe :: String -> Traversal' GenericPackageDescription BuildInfo 
buildInfoOfExe name = condExecutablesL 
                      . traversed 
                      . filtered ((== name) . fst) 
                      . _2 . biplate


buildInfoOfTest:: String -> Traversal' GenericPackageDescription BuildInfo
buildInfoOfTest name = condTestSuitesL
                       . traversed 
                       . filtered ((== name) . fst) 
                       . _2 . biplate 


buildInfoOfBenchm :: String -> Traversal' GenericPackageDescription BuildInfo
buildInfoOfBenchm name = condBenchmarksL
                         . traversed 
                         . filtered ((== name) . fst) 
                         . _2 . biplate


buildInfoOf :: S.Section -> Traversal' GenericPackageDescription BuildInfo
buildInfoOf S.Library           = buildInfoOfLib
buildInfoOf (S.Executable name) = buildInfoOfExe name
buildInfoOf (S.TestSuite name)  = buildInfoOfTest name
buildInfoOf (S.Benchmark name)  = buildInfoOfBenchm name


allBuildInfos :: Traversal' GenericPackageDescription BuildInfo
allBuildInfos = biplate


field :: F.Field -> Traversal' BuildInfo [String]
field F.Hs_Source_Dirs         = hsSourceDirsL
field F.Ghc_Options            = optionsL . traversed . filtered ((== GHC) . fst) . _2
field F.Default_Extensions     = oldAndDefaultExtensionsL . extsToStrings
field F.Default_Language       = error $ "Unexpected argument 'Default_Language' for 'CabalCargs.Lenses.field'!"
field F.Cpp_Options            = cppOptionsL
field F.C_Sources              = cSourcesL
field F.Cc_Options             = ccOptionsL
field F.Extra_Lib_Dirs         = extraLibDirsL
field F.Extra_Libraries        = extraLibsL
field F.Ld_Options             = ldOptionsL
field F.Include_Dirs           = includeDirsL
field F.Includes               = includesL
field F.Package_Db             = error $ "Unexpected argument 'Package_Db' for 'CabalCargs.Lenses.field'!"
field F.Autogen_Hs_Source_Dirs = error $ "Unexpected argument 'Autogen_Hs_Source_Dirs' for 'CabalCargs.Lenses.field'!"
field F.Autogen_Include_Dirs   = error $ "Unexpected argument 'Autogen_Include_Dirs' for 'CabalCargs.Lenses.field'!"
field F.Autogen_Includes       = error $ "Unexpected argument 'Autogen_Includes' for 'CabalCargs.Lenses.field'!"


oldAndDefaultExtensionsL :: Lens' BuildInfo [Extension]
oldAndDefaultExtensionsL = lens getter setter
   where
      getter buildInfo      = (oldExtensions buildInfo) ++ (defaultExtensions buildInfo)
      setter buildInfo exts = buildInfo { defaultExtensions = exts }


extsToStrings :: Iso' [Extension] [String]
extsToStrings = iso (map toString) (map toExt)
   where
      toString ext = 
         case ext of
              EnableExtension knownExt    -> show knownExt
              DisableExtension knownExt   -> "No" ++ show knownExt
              UnknownExtension unknownExt -> unknownExt

      toExt ('N':'o':rest)
         | [(ext, _)] <- reads rest :: [(KnownExtension, String)]
         = DisableExtension ext

      toExt str
         | [(ext, _)] <- reads str :: [(KnownExtension, String)]
         = EnableExtension ext

         | otherwise
         = UnknownExtension str


langToString :: Iso' Language String
langToString = iso toString toLang
   where
      toString lang =
         case lang of
              UnknownLanguage l -> l
              _                 -> show lang

      toLang str
         | [(lang, _)] <- reads str :: [(Language, String)]
         = lang

         | otherwise
         = UnknownLanguage str
