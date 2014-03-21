{-# LANGUAGE TemplateHaskell, Rank2Types, PatternGuards #-}

module CabalCargs.BuildInfo
   ( buildInfosOfLib
   , buildInfosOfExe
   , buildInfosOfTest
   , buildInfosOfBenchmark
   , buildInfosOf
   , buildInfos
   , field
   ) where

import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.Package (Dependency)
import Language.Haskell.Extension
import Control.Lens
import Data.List (find)
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Field as F
import qualified CabalCargs.CondVars as CV


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


buildInfosOf :: S.Section -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOf S.Library           = buildInfosOfLib
buildInfosOf (S.Executable name) = buildInfosOfExe name
buildInfosOf (S.TestSuite name)  = buildInfosOfTest name
buildInfosOf (S.Benchmark name)  = buildInfosOfBenchmark name


buildInfos :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfos vars pkgDescrp =
   concat [ buildInfosOfLib vars pkgDescrp
          , buildInfosOfAllExes vars pkgDescrp
          , buildInfosOfAllTests vars pkgDescrp
          , buildInfosOfAllBenchmarks vars pkgDescrp
          ]


buildInfosOfLib :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfLib vars pkgDescrp
   | Just condLib <- condLibrary pkgDescrp
   = map libBuildInfo $ condTreeDatas vars condLib

   | otherwise
   = []


buildInfosOfExe :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfExe name vars pkgDescrp
   | Just (_, condExe) <- find ((== name) . fst) $ condExecutables pkgDescrp
   = map buildInfo $ condTreeDatas vars condExe

   | otherwise
   = []


buildInfosOfAllExes :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllExes vars pkgDescrp =
   concat $ map ((map buildInfo) . (condTreeDatas vars) . snd) (condExecutables pkgDescrp)


buildInfosOfTest :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfTest name vars pkgDescrp
   | Just (_, condTest) <- find ((== name) . fst) $ condTestSuites pkgDescrp
   = map testBuildInfo $ condTreeDatas vars condTest

   | otherwise
   = []


buildInfosOfAllTests :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllTests vars pkgDescrp =
   concat $ map ((map testBuildInfo) . (condTreeDatas vars) . snd) (condTestSuites pkgDescrp)


buildInfosOfBenchmark :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfBenchmark name vars pkgDescrp
   | Just (_, condBench) <- find ((== name) . fst) $ condBenchmarks pkgDescrp
   = map benchmarkBuildInfo $ condTreeDatas vars condBench

   | otherwise
   = []


buildInfosOfAllBenchmarks :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllBenchmarks vars pkgDescrp =
   concat $ map ((map benchmarkBuildInfo) . (condTreeDatas vars) . snd) (condBenchmarks pkgDescrp)


-- | Returns all 'condTreeData' of the 'CondTree' which conditions match the given 'CondVars'. 
condTreeDatas :: CV.CondVars -> CondTree ConfVar [Dependency] a -> [a]
condTreeDatas vars tree = go (condTreeComponents tree) [condTreeData tree]
   where
      go [] dats = dats

      go ((cond, ifTree, elseTree) : comps) dats
         | CV.eval vars cond
         = go comps $ go (condTreeComponents ifTree) (condTreeData ifTree : dats)

         | Just tree <- elseTree
         = go comps $ go (condTreeComponents tree) (condTreeData tree : dats)

         | otherwise
         = go comps dats


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
field F.Hdevtools_Socket       = nopLens


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
