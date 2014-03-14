{-# LANGUAGE TemplateHaskell, Rank2Types, PatternGuards #-}

module CabalCargs.Lenses
   ( buildInfoOfLib
   , buildInfoOfExe
   , buildInfoOfTest
   , buildInfoOfBenchm
   , buildInfoOf
   , allBuildInfos
   , field
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
              , ("defaultLanguage"  , "defaultLanguageL")
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


-- | A traversal to visit all 'BuildInfo' of the 'GenericPackageDescription',
--   this includes all sections and also the conditional branches in the cabal file.
allBuildInfos :: Traversal' GenericPackageDescription BuildInfo
allBuildInfos = biplate


-- | A lens from a 'BuildInfo' to a list of stringified field entries of the 'BuildInfo'.
field :: F.Field -> Traversal' BuildInfo [String]
field F.Hs_Source_Dirs         = hsSourceDirsL
field F.Ghc_Options            = optionsL . traversed . filtered ((== GHC) . fst) . _2
field F.Default_Extensions     = oldAndDefaultExtensionsL . extsToStrings
field F.Default_Language       = defaultLanguageL . langToString
field F.Cpp_Options            = cppOptionsL
field F.C_Sources              = cSourcesL
field F.Cc_Options             = ccOptionsL
field F.Extra_Lib_Dirs         = extraLibDirsL
field F.Extra_Libraries        = extraLibsL
field F.Ld_Options             = ldOptionsL
field F.Include_Dirs           = includeDirsL
field F.Includes               = includesL
field F.Package_Db             = nopLens
field F.Autogen_Hs_Source_Dirs = nopLens
field F.Autogen_Include_Dirs   = nopLens
field F.Autogen_Includes       = nopLens


-- | A lens that merges the fields 'default-extensions' and 'extensions',
--   which now mean the same thing in cabal, 'extensions' is only the old
--   name of 'default-extensions'.
oldAndDefaultExtensionsL :: Lens' BuildInfo [Extension]
oldAndDefaultExtensionsL = lens getter setter
   where
      getter buildInfo      = (oldExtensions buildInfo) ++ (defaultExtensions buildInfo)
      setter buildInfo exts = buildInfo { defaultExtensions = exts }


-- | A lens (iso) that converts between a list of extensions
--   and a list of strings containing the names of the extensions.
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


-- | A lens (iso) that converts between the language and
--   a list containing a string with the name of the language.
langToString :: Iso' (Maybe Language) [String]
langToString = iso toString toLang
   where
      toString Nothing     = []
      toString (Just lang) =
         case lang of
              UnknownLanguage l -> [l]
              _                 -> [show lang]

      toLang (str:[])
         | [(lang, _)] <- reads str :: [(Language, String)]
         = Just lang

         | otherwise
         = Just $ UnknownLanguage str

      toLang _ = Nothing


-- | A lens that does nothing, always returns an empty
--   list and doesn't modify the given BuildInfo.
nopLens :: Lens' BuildInfo [String]
nopLens = lens (const []) (\buildInfo _ -> buildInfo)
