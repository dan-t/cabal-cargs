{-# Language PatternGuards #-}

module CabalCargs.Spec
   ( Spec(..)
   , fromCabalFile
   , fromSourceFile
   , fromCmdArgs
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import CabalCargs.Args (Args)
import qualified CabalCargs.Lenses as L
import qualified CabalCargs.Args as A
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Fields as F
import qualified System.IO.Strict as Strict
import Control.Monad.Trans.Either (EitherT, left, right)
import Control.Monad.IO.Class
import Control.Monad (filterM)
import Control.Applicative ((<$>))
import Control.Lens
import System.Directory (getCurrentDirectory)
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as FS
import qualified Data.Text as T
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)


-- | Specifies which compiler args from which sections should be collected.
data Spec = Spec 
   { sections     :: S.Sections                -- ^ the sections used for collecting the compiler args
   , fields       :: F.Fields                  -- ^ for these fields compiler args are collected
   , cabalPackage :: GenericPackageDescription -- ^ the package description of the read in cabal file
   , cabalFile    :: FilePath                  -- ^ the cabal file read from
   , packageDB    :: Maybe FilePath            -- ^ path to the package database of the cabal sandbox
   }


type Error = String
io = liftIO 


-- | Create a 'Spec' from the given cabal file, sections and fields.
--
--   If a cabal sandbox is present in the directory of the cabal file, then
--   the path to its package database is also returned.
fromCabalFile :: FilePath -> S.Sections -> F.Fields -> EitherT Error IO Spec
fromCabalFile file sections fields = do
   pkgDB     <- io $ findPackageDB file
   pkgDescrp <- packageDescription file
   absFile   <- FP.encodeString <$> (io $ absoluteFile file)
   right $ Spec
      { sections     = sections
      , fields       = fields
      , cabalPackage = pkgDescrp
      , cabalFile    = absFile
      , packageDB    = pkgDB
      }


-- | Create a 'Spec' from the given source file and fields.
--
--   Starting at the directory of the source file a cabal file is searched
--   upwards the directory tree.
--
--   The found cabal file is searched for a fitting section for the source file.
--   If no fitting section could be found, then all sections are used.
--
--   If a cabal sandbox is present in the directory of the cabal file, then
--   the path to its package database is also returned.
fromSourceFile :: FilePath -> F.Fields -> EitherT Error IO Spec
fromSourceFile file fields = do
   cabalFile   <- findCabalFile file
   pkgDB       <- io $ findPackageDB cabalFile
   pkgDescrp   <- packageDescription cabalFile
   srcSections <- io $ findSections file cabalFile pkgDescrp
   right $ Spec
      { sections = combineSections S.AllSections srcSections
      , fields   = fields
      , cabalPackage = pkgDescrp
      , cabalFile    = cabalFile
      , packageDB    = pkgDB
      }


-- | Create a 'Spec' by the command line arguments given to 'cabal-cargs'.
--
--   Depending on the command line arguments 'fromCmdArgs' might behave like
--   'fromCabalFile', if only a cabal file was given, like 'fromSourceFile',
--   if only a source file was given or like a mix of both, if a cabal file
--   and a source file have been given.
fromCmdArgs :: Args -> EitherT Error IO Spec
fromCmdArgs args
   | Just cabalFile <- A.cabalFile args = do
      spec        <- fromCabalFile cabalFile (S.sections args) (F.fields args)
      srcSections <- io $ case A.sourceFile args of
                               Just srcFile -> findSections srcFile cabalFile (cabalPackage spec)
                               _            -> return []

      right $ spec { sections = combineSections (sections spec) srcSections }

   | Just sourceFile <- A.sourceFile args = do
      spec <- fromSourceFile sourceFile (F.fields args)
      let specSections = case sections spec of
                              S.Sections ss -> ss
                              _             -> []

      right $ spec { sections = combineSections (S.sections args) specSections }

   | otherwise = do
      curDir    <- io $ getCurrentDirectory
      cabalFile <- findCabalFile (curDir ++ "/")
      fromCabalFile cabalFile (S.sections args) (F.fields args)


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
      fittingSection srcFile cabalDir (_, []) =
         isJust $ FP.stripPrefix (cabalDir </> FP.empty) srcFile

      fittingSection srcFile cabalDir (_, srcDirs) = any samePrefix srcDirs
         where samePrefix srcDir = isJust $ FP.stripPrefix (cabalDir </> srcDir </> FP.empty) srcFile


type HsSourceDirs = [FP.FilePath]
-- | Returns the hs-source-dirs of all sections present in the given package description.
allHsSourceDirs :: GenericPackageDescription -> [(S.Section, HsSourceDirs)]
allHsSourceDirs pkgDescrp = map fromBuildInfo buildInfos
   where
      fromBuildInfo (section, buildInfo) =
         (section, toFPs $ PD.hsSourceDirs (pkgDescrp ^. buildInfo))

      buildInfos = concat [ [ (S.Library, L.buildInfoOfLib) | isJust $ PD.condLibrary pkgDescrp ]
                          , map fromExe (PD.condExecutables pkgDescrp)
                          , map fromTest (PD.condTestSuites pkgDescrp)
                          , map fromBenchm (PD.condBenchmarks pkgDescrp)
                          ]

      fromExe (name, _)    = (S.Executable name, L.buildInfoOfExe name)
      fromTest (name, _)   = (S.TestSuite name, L.buildInfoOfTest name)
      fromBenchm (name, _) = (S.Benchmark name, L.buildInfoOfBenchm name)

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


combineSections :: S.Sections -> [S.Section] -> S.Sections
combineSections S.AllSections     [] = S.AllSections
combineSections S.AllSections     ss = S.Sections ss
combineSections (S.Sections ss)    _ = S.Sections ss
