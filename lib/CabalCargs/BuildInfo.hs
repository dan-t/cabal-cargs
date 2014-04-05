{-# LANGUAGE TemplateHaskell, Rank2Types, PatternGuards #-}

module CabalCargs.BuildInfo
   ( BuildInfo(..)
   , buildInfosOfLib
   , buildInfosOfExe
   , buildInfosOfTest
   , buildInfosOfBenchmark
   , buildInfosOf
   , buildInfos
   , field
   ) where

import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription (GenericPackageDescription(..), CondTree(..), ConfVar)
import Distribution.Compiler
import Distribution.Package (Dependency(..), PackageName(..))
import Distribution.Version (anyVersion)
import Language.Haskell.Extension
import Control.Lens
import Data.List (find)
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Fields as F
import qualified CabalCargs.CondVars as CV


data BuildInfo = BuildInfo
   { buildInfo    :: PD.BuildInfo
   , buildDepends :: [Dependency]
   } deriving (Show, Eq)


makeLensesFor [ ("buildInfo"   , "buildInfoL")
              , ("buildDepends", "buildDependsL")
              ] ''BuildInfo


makeLensesFor [ ("hsSourceDirs"      , "hsSourceDirsL")
              , ("options"           , "optionsL")
              , ("defaultLanguage"   , "defaultLanguageL")
              , ("cppOptions"        , "cppOptionsL")
              , ("cSources"          , "cSourcesL")
              , ("ccOptions"         , "ccOptionsL")
              , ("extraLibDirs"      , "extraLibDirsL")
              , ("extraLibs"         , "extraLibsL")
              , ("ldOptions"         , "ldOptionsL")
              , ("includeDirs"       , "includeDirsL")
              , ("includes"          , "includesL")
              ] ''PD.BuildInfo


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
   = map (toBuildInfo PD.libBuildInfo) $ condTreeDatasAndConstraints vars condLib

   | otherwise
   = []


buildInfosOfExe :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfExe name vars pkgDescrp
   | Just (_, condExe) <- find ((== name) . fst) $ condExecutables pkgDescrp
   = map (toBuildInfo PD.buildInfo) $ condTreeDatasAndConstraints vars condExe

   | otherwise
   = []


buildInfosOfAllExes :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllExes vars pkgDescrp =
   concat $ map ((map (toBuildInfo PD.buildInfo)) . (condTreeDatasAndConstraints vars) . snd) (condExecutables pkgDescrp)


buildInfosOfTest :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfTest name vars pkgDescrp
   | Just (_, condTest) <- find ((== name) . fst) $ condTestSuites pkgDescrp
   = map (toBuildInfo PD.testBuildInfo) $ condTreeDatasAndConstraints vars condTest

   | otherwise
   = []


buildInfosOfAllTests :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllTests vars pkgDescrp =
   concat $ map ((map (toBuildInfo PD.testBuildInfo)) . (condTreeDatasAndConstraints vars) . snd) (condTestSuites pkgDescrp)


buildInfosOfBenchmark :: String -> CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfBenchmark name vars pkgDescrp
   | Just (_, condBench) <- find ((== name) . fst) $ condBenchmarks pkgDescrp
   = map (toBuildInfo PD.benchmarkBuildInfo) $ condTreeDatasAndConstraints vars condBench

   | otherwise
   = []


buildInfosOfAllBenchmarks :: CV.CondVars -> GenericPackageDescription -> [BuildInfo]
buildInfosOfAllBenchmarks vars pkgDescrp =
   concat $ map ((map (toBuildInfo PD.benchmarkBuildInfo)) . (condTreeDatasAndConstraints vars) . snd) (condBenchmarks pkgDescrp)


toBuildInfo :: (dat -> PD.BuildInfo) -> (dat, [Dependency]) -> BuildInfo
toBuildInfo f (dat, deps) = BuildInfo { buildInfo    = f dat
                                      , buildDepends = deps
                                      }


-- | Returns all 'condTreeData' and 'condTreeConstraints' of the 'CondTree' which conditions match the given 'CondVars'.
condTreeDatasAndConstraints :: CV.CondVars -> CondTree ConfVar [Dependency] dat -> [(dat, [Dependency])]
condTreeDatasAndConstraints vars tree = go (condTreeComponents tree) [dataAndConstraints tree]
   where
      go [] dats = dats

      go ((cond, ifTree, elseTree) : comps) dats
         | CV.eval vars cond
         = go comps $ go (condTreeComponents ifTree) (dataAndConstraints ifTree : dats)

         | Just tree <- elseTree
         = go comps $ go (condTreeComponents tree) (dataAndConstraints tree : dats)

         | otherwise
         = go comps dats

      dataAndConstraints tree = (condTreeData tree, condTreeConstraints tree)


-- | A lens from a 'BuildInfo' to a list of stringified field entries of the 'BuildInfo'.
field :: F.Field -> Traversal' BuildInfo [String]
field F.Hs_Source_Dirs         = buildInfoL . hsSourceDirsL
field F.Ghc_Options            = buildInfoL . optionsL . traversed . filtered ((== GHC) . fst) . _2
field F.Default_Extensions     = buildInfoL . oldAndDefaultExtensionsL . extsToStrings
field F.Default_Language       = buildInfoL . defaultLanguageL . langToString
field F.Cpp_Options            = buildInfoL . cppOptionsL
field F.C_Sources              = buildInfoL . cSourcesL
field F.Cc_Options             = buildInfoL . ccOptionsL
field F.Extra_Lib_Dirs         = buildInfoL . extraLibDirsL
field F.Extra_Libraries        = buildInfoL . extraLibsL
field F.Ld_Options             = buildInfoL . ldOptionsL
field F.Include_Dirs           = buildInfoL . includeDirsL
field F.Includes               = buildInfoL . includesL
field F.Build_Depends          = buildDependsL . depsToStrings
field F.Package_Db             = nopLens
field F.Autogen_Hs_Source_Dirs = nopLens
field F.Autogen_Include_Dirs   = nopLens
field F.Autogen_Includes       = nopLens
field F.Hdevtools_Socket       = nopLens


-- | A lens that merges the fields 'default-extensions' and 'extensions',
--   which now mean the same thing in cabal, 'extensions' is only the old
--   name of 'default-extensions'.
oldAndDefaultExtensionsL :: Lens' PD.BuildInfo [Extension]
oldAndDefaultExtensionsL = lens getter setter
   where
      getter buildInfo      = (PD.oldExtensions buildInfo) ++ (PD.defaultExtensions buildInfo)
      setter buildInfo exts = buildInfo { PD.defaultExtensions = exts }


-- | An iso that converts between a list of extensions
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


-- | An iso that converts between the language and
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


-- | An iso that converts a list of dependencies to a list of package names
depsToStrings :: Iso' [Dependency] [String]
depsToStrings = iso (map toString) (map toDep)
   where
      toString (Dependency (PackageName name) _) = name
      toDep name = Dependency (PackageName name) anyVersion


-- | A lens that does nothing, always returns an empty
--   list and doesn't modify the given BuildInfo.
nopLens :: Lens' BuildInfo [String]
nopLens = lens (const []) (\buildInfo _ -> buildInfo)
