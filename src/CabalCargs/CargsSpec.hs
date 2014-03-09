{-# Language PatternGuards #-}

module CabalCargs.CargsSpec
   ( CargsSpec(..)
   , cargsSpec
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import CabalCargs.Args (Args)
import qualified CabalCargs.Args as A
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Fields as F
import qualified System.IO.Strict as Strict
import Control.Monad.Trans.Either (EitherT, left, right)
import Control.Monad.IO.Class
import Control.Monad (filterM)
import Control.Applicative ((<$>))
import System.Directory (getCurrentDirectory)
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as FS
import qualified Data.Text as T
import Data.List (find, nub)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)


-- | Specifies which compiler args from which sections should be collected.
data CargsSpec = CargsSpec 
   { sections     :: S.Sections                -- ^ the sections used for collecting the compiler args
   , fields       :: F.Fields                  -- ^ for these fields compiler args are collected
   , cabalPackage :: GenericPackageDescription -- ^ the package description of the read in cabal file
   , cabalFile    :: FilePath                  -- ^ the cabal file read from
   , packageDB    :: Maybe FilePath            -- ^ path to the package database of the cabal sandbox
   }


type Error = String
io = liftIO 

-- | Create a 'CargsSpec' by the command line arguments given to 'cabal-cargs'.
cargsSpec :: Args -> EitherT Error IO CargsSpec
cargsSpec args
   | Just cabalFile <- A.cabalFile args = do
      pkgDB       <- io $ findPackageDB cabalFile
      pkgDescrp   <- packageDescription cabalFile
      srcSections <- io $ case A.sourceFile args of
                               Just srcFile -> findSections srcFile cabalFile pkgDescrp
                               _            -> return []
      right $ CargsSpec 
         { sections     = combineSections (S.sections args) srcSections
         , fields       = F.fields args
         , cabalPackage = pkgDescrp
         , cabalFile    = cabalFile
         , packageDB    = pkgDB
         }

   | Just sourceFile <- A.sourceFile args = do
      cabalFile   <- findCabalFile sourceFile
      pkgDB       <- io $ findPackageDB cabalFile
      pkgDescrp   <- packageDescription cabalFile
      srcSections <- io $ findSections sourceFile cabalFile pkgDescrp
      right $ CargsSpec 
         { sections     = combineSections (S.sections args) srcSections
         , fields       = F.fields args
         , cabalPackage = pkgDescrp
         , cabalFile    = cabalFile
         , packageDB    = pkgDB
         }

   | otherwise = do
      curDir    <- io $ getCurrentDirectory
      cabalFile <- findCabalFile curDir
      pkgDB     <- io $ findPackageDB cabalFile
      pkgDescrp <- packageDescription cabalFile
      right $ CargsSpec 
         { sections     = S.sections args
         , fields       = F.fields args
         , cabalPackage = pkgDescrp
         , cabalFile    = cabalFile
         , packageDB    = pkgDB
         }

   where
      combineSections S.AllSections     [] = S.AllSections
      combineSections S.AllSections     ss = S.Sections ss
      combineSections (S.Sections ss1) ss2 = S.Sections $ nub $ ss1 ++ ss2


packageDescription :: FilePath -> EitherT Error IO GenericPackageDescription
packageDescription file = do
   contents <- io $ Strict.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> left $ show error
        ParseOk _ pkgDescrp -> right pkgDescrp


-- | Find matching sections in the package description for the given source file.
--   This is done by checking if the source file is contained in the directory
--   or a sub directory of the directories listed in the 'hs-source-dirs' field
--   of the section.
findSections :: FilePath -> FilePath -> GenericPackageDescription -> IO [S.Section]
findSections srcFile cabalFile pkgDescrp = do
   absSrcFile <- absoluteFile srcFile
   cabalDir   <- absoluteDirectory cabalFile
   let sections = filter (fittingSection absSrcFile cabalDir) (allHsSourceDirs pkgDescrp)   
   return $ map fst sections

   where
      fittingSection srcFile cabalDir (_, srcDirs) = any samePrefix srcDirs
         where samePrefix srcDir = isJust $ FP.stripPrefix (cabalDir </> srcDir </> FP.empty) srcFile


type HsSourceDirs = [FP.FilePath]
-- | Returns the hs-source-dirs of all sections present in the given package description.
allHsSourceDirs :: GenericPackageDescription -> [(S.Section, HsSourceDirs)]
allHsSourceDirs pkgDescrp =
   concat [ libraryHsSourceDirs
          , exeHsSourceDirs
          , testHsSourceDirs
          , benchmHsSourceDirs
          ]
   where
      libraryHsSourceDirs
         | Just lib <- PD.condLibrary pkgDescrp
         = [ (S.Library, toFPs . PD.hsSourceDirs . PD.libBuildInfo . PD.condTreeData $ lib) ]

         | otherwise
         = []

      exeHsSourceDirs = map fromExe $ PD.condExecutables pkgDescrp
         where fromExe (name, exe) =
                  (S.Executable name, toFPs . PD.hsSourceDirs . PD.buildInfo . PD.condTreeData $ exe)

      testHsSourceDirs = map fromTest $ PD.condTestSuites pkgDescrp
         where fromTest (name, test) = 
                  (S.TestSuite name, toFPs . PD.hsSourceDirs . PD.testBuildInfo. PD.condTreeData $ test)

      benchmHsSourceDirs = map fromBenchm $ PD.condBenchmarks pkgDescrp
         where fromBenchm (name, benchm) =
                  (S.Benchmark name, toFPs . PD.hsSourceDirs . PD.benchmarkBuildInfo . PD.condTreeData $ benchm)

      toFPs = map FP.decodeString


-- | Find the package database of the cabal sandbox from the given cabal file.
--   The returned file path is absolute.
findPackageDB :: FilePath -> IO (Maybe FilePath)
findPackageDB cabalFile = do
   cabalDir <- absoluteDirectory cabalFile
   files    <- filterM FS.isDirectory =<< (FS.listDirectory (cabalDir </> sandboxDir))
   return $ FP.encodeString <$> find isPackageDB files

   where
      isPackageDB file
         | comps@(_:_) <- splitOn "-" (FP.encodeString file)
         = last comps == "packages.conf.d"

         | otherwise
         = False

      sandboxDir = FP.decodeString ".cabal-sandbox"


-- | Find a cabal file starting at the given directory, going upwards the directory
--   tree until a cabal file could be found. The returned file path is absolute.
findCabalFile :: FilePath -> EitherT Error IO FilePath
findCabalFile file = do
   cabalFile <- io $ do
      dir <- absoluteDirectory file
      findCabalFile' dir

   if cabalFile == FP.empty
      then left "Couldn't find Cabal file!"
      else right . FP.encodeString $ cabalFile

   where
      findCabalFile' dir = do
         files <- filterM FS.isFile =<< (FS.listDirectory dir)
         case find isCabalFile files of
              Just file -> return $ dir </> file
              _         -> do
                 let parent = FP.parent dir
                 if parent == dir
                    then return FP.empty
                    else findCabalFile' parent

      isCabalFile file
         | Just ext <- FP.extension file
         = ext == cabalExt

         | otherwise
         = False

      cabalExt = T.pack "cabal"


absoluteDirectory :: FilePath -> IO FP.FilePath
absoluteDirectory = FS.canonicalizePath . FP.directory . FP.decodeString


absoluteFile :: FilePath -> IO FP.FilePath
absoluteFile = FS.canonicalizePath . FP.decodeString
