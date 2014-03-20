{-# Language PatternGuards #-}

module CabalCargs.Spec
   ( Spec(..)
   , fromCmdArgs
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import qualified Distribution.System as Sys
import CabalCargs.Args (Args)
import qualified CabalCargs.BuildInfo as B
import qualified CabalCargs.Args as A
import qualified CabalCargs.Sections as S
import qualified CabalCargs.Fields as F
import qualified CabalCargs.CondVars as CV
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
import Data.List (find, isSuffixOf, isPrefixOf)
import Data.Maybe (isJust)


-- | Specifies which compiler args from which sections should be collected.
data Spec = Spec 
   { sections      :: S.Sections                -- ^ the sections used for collecting the compiler args
   , fields        :: F.Fields                  -- ^ for these fields compiler args are collected
   , condVars      :: CV.CondVars               -- ^ used for the evaluation of the conditional fields in the cabal file
   , cabalPackage  :: GenericPackageDescription -- ^ the package description of the read in cabal file
   , cabalFile     :: FilePath                  -- ^ the cabal file read from
   , distDir       :: Maybe FilePath            -- ^ the dist directory of the cabal build, a relative path to the directory of the cabal file
   , packageDB     :: Maybe FilePath            -- ^ the directory of package database of the cabal sandbox, a relative path to the directory of the cabal file
   , relativePaths :: Bool                      -- ^ if all returned paths are relative to the directory of the cabal file, otherwise all paths are absolute
   }


type Error = String
io = liftIO 


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

      right $ applyCondVars $ spec { sections      = combineSections (sections spec) srcSections
                                   , relativePaths = A.relative args
                                   }

   | Just sourceFile <- A.sourceFile args = do
      spec <- fromSourceFile sourceFile (F.fields args)
      let specSections = case sections spec of
                              S.Sections ss -> ss
                              _             -> []

      right $ applyCondVars $ spec { sections      = combineSections (S.sections args) specSections
                                   , relativePaths = A.relative args
                                   }

   | otherwise = do
      curDir    <- io $ getCurrentDirectory
      cabalFile <- findCabalFile curDir
      spec      <- fromCabalFile cabalFile (S.sections args) (F.fields args)
      right $ applyCondVars $ spec { relativePaths = A.relative args }

   where
      applyCondVars = applyFlags args . applyOS args . applyArch args



-- | Create a 'Spec' from the given cabal file, sections and fields.
--
--   If a cabal sandbox is present in the directory of the cabal file, then
--   the path to its package database is also returned.
fromCabalFile :: FilePath -> S.Sections -> F.Fields -> EitherT Error IO Spec
fromCabalFile file sections fields = do
   pkgDB     <- io $ findPackageDB file
   distDir   <- io $ findDistDir file
   pkgDescrp <- packageDescription file
   absFile   <- FP.encodeString <$> (io $ absoluteFile file)
   right $ Spec
      { sections      = sections
      , fields        = fields
      , condVars      = CV.fromDefaults pkgDescrp
      , cabalPackage  = pkgDescrp
      , cabalFile     = absFile
      , distDir       = distDir
      , packageDB     = pkgDB
      , relativePaths = False
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
   distDir     <- io $ findDistDir cabalFile
   pkgDescrp   <- packageDescription cabalFile
   srcSections <- io $ findSections file cabalFile pkgDescrp
   right $ Spec
      { sections = combineSections S.AllSections srcSections
      , fields        = fields
      , condVars      = CV.fromDefaults pkgDescrp
      , cabalPackage  = pkgDescrp
      , cabalFile     = cabalFile
      , distDir       = distDir
      , packageDB     = pkgDB
      , relativePaths = False
      }


applyFlags :: Args -> Spec -> Spec
applyFlags args spec =
   spec { condVars = disableFlags . enableFlags $ condVars spec }
   where
      disableFlags condVars = foldr CV.disableFlag condVars (A.disable args)
      enableFlags  condVars = foldr CV.enableFlag condVars (A.enable args)


applyOS :: Args -> Spec -> Spec
applyOS (A.Args { A.os = os }) spec
   | Just str    <- os
   , [(name, _)] <- reads str :: [(Sys.OS, String)]
   = setOS name

   | Just str    <- os
   = setOS $ Sys.OtherOS str

   | otherwise
   = spec

   where
      setOS name = spec { condVars = (condVars spec) { CV.os = name } }


applyArch :: Args -> Spec -> Spec
applyArch (A.Args { A.arch = arch }) spec
   | Just str    <- arch
   , [(name, _)] <- reads str :: [(Sys.Arch, String)]
   = setArch name

   | Just str    <- arch
   = setArch $ Sys.OtherArch str

   | otherwise
   = spec

   where
      setArch name = spec { condVars = (condVars spec) { CV.arch = name } }


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
      fromBuildInfo (section, buildInfos) =
         (section, toFPs $ concat $ (map PD.hsSourceDirs) (buildInfos condVars pkgDescrp))

      buildInfos = concat [ [ (S.Library, B.buildInfosOfLib) | isJust $ PD.condLibrary pkgDescrp ]
                          , map fromExe (PD.condExecutables pkgDescrp)
                          , map fromTest (PD.condTestSuites pkgDescrp)
                          , map fromBenchm (PD.condBenchmarks pkgDescrp)
                          ]

      fromExe (name, _)    = (S.Executable name, B.buildInfosOfExe name)
      fromTest (name, _)   = (S.TestSuite name, B.buildInfosOfTest name)
      fromBenchm (name, _) = (S.Benchmark name, B.buildInfosOfBenchmark name)

      toFPs = map FP.decodeString

      condVars = CV.fromDefaults pkgDescrp


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


-- | Find the package database of the cabal sandbox from the given cabal file.
--   The returned file path is relative to the directory of the cabal file.
findPackageDB :: FilePath -> IO (Maybe FilePath)
findPackageDB cabalFile = do
   cabalDir <- absoluteDirectory cabalFile
   let sandboxDir = cabalDir </> FP.decodeString ".cabal-sandbox"
   hasDir <- FS.isDirectory sandboxDir
   if hasDir
      then do
         files <- filterM FS.isDirectory =<< (FS.listDirectory sandboxDir)
         return $ FP.encodeString . (stripPrefix cabalDir) <$> find isPackageDB files
      else return Nothing

   where
      isPackageDB file = "packages.conf.d" `isSuffixOf` (FP.encodeString file)


-- | Find the dist directory of the cabal build from the given cabal file. For a non sandboxed
--   build it's just the directory 'dist' in the cabal build directory. For a sandboxed build
--   it's the directory 'dist/dist-sandbox-*'. The returned file path is relative to the
--   directory of the cabal file.
findDistDir :: FilePath -> IO (Maybe FilePath)
findDistDir cabalFile = do
   cabalDir   <- absoluteDirectory cabalFile
   let distDir = cabalDir </> FP.decodeString "dist"
   hasDistDir <- FS.isDirectory distDir
   if hasDistDir
      then do
         files <- filterM FS.isDirectory =<< (FS.listDirectory distDir)
         return $ FP.encodeString . (stripPrefix cabalDir) <$> maybe (Just distDir) Just (find isSandboxDistDir files)
      else return Nothing

   where
      isSandboxDistDir file =
         "dist-sandbox-" `isPrefixOf` (FP.encodeString . FP.filename $ file)


absoluteDirectory :: FilePath -> IO FP.FilePath
absoluteDirectory file = do
   absFile <- absoluteFile file
   isDir   <- FS.isDirectory absFile
   if isDir
      then return absFile
      else return . FP.directory $ absFile


absoluteFile :: FilePath -> IO FP.FilePath
absoluteFile = FS.canonicalizePath . FP.decodeString


stripPrefix :: FP.FilePath -> FP.FilePath -> FP.FilePath
stripPrefix prefix file
   | Just stripped <- FP.stripPrefix prefix file
   = stripped

   | otherwise
   = file


combineSections :: S.Sections -> [S.Section] -> S.Sections
combineSections S.AllSections     [] = S.AllSections
combineSections S.AllSections     ss = S.Sections ss
combineSections (S.Sections ss)    _ = S.Sections ss
