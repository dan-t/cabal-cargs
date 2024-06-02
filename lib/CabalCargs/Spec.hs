{-# Language PatternGuards, CPP #-}

module CabalCargs.Spec
   ( Spec(..)
   , fromCmdArgs
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Warning (PWarning)
import qualified Distribution.System as Sys
import CabalCargs.Args (Args)
import qualified CabalCargs.Args as A
import qualified CabalCargs.Fields as F
import qualified CabalLenses as CL
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class
import Control.Lens
import System.Directory (getCurrentDirectory)
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as FS
import Data.List ((\\))
import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.ByteString as BS

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


-- | Specifies which compiler args from which sections should be collected.
data Spec = Spec
   { sections      :: [CL.Section]              -- ^ the sections used for collecting the compiler args
   , fields        :: F.Fields                  -- ^ for these fields compiler args are collected
   , condVars      :: CL.CondVars               -- ^ used for the evaluation of the conditional fields in the cabal file
   , pkgDescrp     :: GenericPackageDescription -- ^ the package description of the read in cabal file
   , cabalFile     :: FilePath                  -- ^ the cabal file read from
   , distDir       :: Maybe FilePath            -- ^ the dist directory of the cabal build
   , packageDB     :: Maybe FilePath            -- ^ the directory of package database of the cabal sandbox
   , relativePaths :: Bool                      -- ^ if all returned paths are relative to the directory of the cabal file, otherwise all paths are absolute
   }


type Error = String

io :: MonadIO m => IO a -> m a
io = liftIO


-- | Create a 'Spec' by the command line arguments given to 'cabal-cargs'.
--
--   Depending on the command line arguments 'fromCmdArgs' might behave like
--   'fromCabalFile', if only a cabal file was given, like 'fromSourceFile',
--   if only a source file was given or like a mix of both, if a cabal file
--   and a source file have been given.
fromCmdArgs :: Args -> IO (Either Error Spec)
fromCmdArgs args
   | Just cabalFile <- A.cabalFile args = runExceptT $ do
      spec        <- fromCabalFile cabalFile
      srcSections <- io $ case A.sourceFile args of
                               Just srcFile -> findSections srcFile cabalFile (pkgDescrp spec)
                               _            -> return []

      return $ applyCondVars $ spec { sections      = combineSections (args, pkgDescrp spec) srcSections
                                    , fields        = fields_ args
                                    , relativePaths = A.relative args
                                    }

   | Just sourceFile <- A.sourceFile args = runExceptT $ do
      spec <- fromSourceFile sourceFile
      return $ applyCondVars $ spec { sections      = combineSections (args, pkgDescrp spec) (sections spec)
                                    , fields        = fields_ args
                                    , relativePaths = A.relative args
                                    }

   | otherwise = runExceptT $ do
      curDir    <- io getCurrentDirectory
      cabalFile <- CL.findCabalFile curDir
      spec      <- fromCabalFile cabalFile
      return $ applyCondVars $ spec { sections      = sections_ args (pkgDescrp spec)
                                    , fields        = fields_ args
                                    , relativePaths = A.relative args
                                    }

   where
      applyCondVars = applyFlags args . applyOS args . applyArch args



-- | Create a 'Spec' from the given cabal file.
--
--   If a cabal sandbox is present in the directory of the cabal file, then
--   the path to its package database is also returned.
fromCabalFile :: FilePath -> ExceptT Error IO Spec
fromCabalFile file = do
   pkgDescrp <- packageDescription file
   pkgDB     <- CL.findPackageDB file
   distDir   <- io $ CL.findDistDir file
   absFile   <- FP.encodeString <$> io (absoluteFile file)
   return $ Spec
      { sections      = CL.allSections pkgDescrp
      , fields        = F.allFields
      , condVars      = CL.fromDefaults pkgDescrp
      , pkgDescrp     = pkgDescrp
      , cabalFile     = absFile
      , distDir       = distDir
      , packageDB     = pkgDB
      , relativePaths = False
      }


-- | Create a 'Spec' from the given source file.
--
--   Starting at the directory of the source file a cabal file is searched
--   upwards the directory tree.
--
--   The found cabal file is searched for a fitting section for the source file.
--   If no fitting section could be found, then all sections are used.
--
--   If a cabal sandbox is present in the directory of the cabal file, then
--   the path to its package database is also returned.
fromSourceFile :: FilePath -> ExceptT Error IO Spec
fromSourceFile file = do
   cabalFile   <- CL.findCabalFile file
   pkgDB       <- CL.findPackageDB cabalFile
   distDir     <- io $ CL.findDistDir cabalFile
   pkgDescrp   <- packageDescription cabalFile
   srcSections <- io $ findSections file cabalFile pkgDescrp
   return $ Spec
      { sections      = srcSections
      , fields        = F.allFields
      , condVars      = CL.fromDefaults pkgDescrp
      , pkgDescrp     = pkgDescrp
      , cabalFile     = cabalFile
      , distDir       = distDir
      , packageDB     = pkgDB
      , relativePaths = False
      }


applyFlags :: Args -> Spec -> Spec
applyFlags args spec =
   spec { condVars = disableFlags . enableFlags $ condVars spec }
   where
      disableFlags condVars = foldr CL.disableFlag condVars (A.disable args)
      enableFlags  condVars = foldr CL.enableFlag condVars (A.enable args)


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
      setOS name = spec { condVars = (condVars spec) { CL.os = name } }


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
      setArch name = spec { condVars = (condVars spec) { CL.arch = name } }


packageDescription :: FilePath -> ExceptT Error IO GenericPackageDescription
packageDescription file = do
   contents <- io $ BS.readFile file
   let (warnings, result) = runParseResult $ parseGenericPackageDescription contents
   io $ showWarnings warnings
   case result of
        Left (_, errors) -> throwE $ show errors
        Right pkgDescrp  -> return pkgDescrp

   where
      showWarnings :: [PWarning] -> IO ()
      showWarnings [] = return ()
      showWarnings ws = putStrLn $ "cabal-cargs: " ++ (L.intercalate ", " $ map show ws)


-- | Find matching sections in the package description for the given source file.
--   This is done by checking if the source file is contained in the directory
--   or a sub directory of the directories listed in the 'hs-source-dirs' field
--   of the section.
findSections :: FilePath -> FilePath -> GenericPackageDescription -> IO [CL.Section]
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
allHsSourceDirs :: GenericPackageDescription -> [(CL.Section, HsSourceDirs)]
allHsSourceDirs pkgDescrp = zip sections hsSourceDirs
   where
      sections     = CL.allSections pkgDescrp
      hsSourceDirs = map (\section -> toFPs $ pkgDescrp ^. CL.buildInfoIf condVars section . CL.hsSourceDirsL . CL.symPathsToFilePaths) sections
         where
            toFPs    = map FP.decodeString
            condVars = CL.fromDefaults pkgDescrp


absoluteDirectory :: FilePath -> IO FP.FilePath
absoluteDirectory file = do
   absFile <- absoluteFile file
   isDir   <- FS.isDirectory absFile
   if isDir
      then return absFile
      else return . FP.directory $ absFile


absoluteFile :: FilePath -> IO FP.FilePath
absoluteFile = FS.canonicalizePath . FP.decodeString


combineSections :: (Args, GenericPackageDescription) -> [CL.Section] -> [CL.Section]
combineSections (args, pkgDescrp) sections
   | A.allSections args
   = CL.allSections pkgDescrp

   | [] <- explicitSections args
   , null sections
   = CL.allSections pkgDescrp

   | otherwise
   = L.nub $ explicitSections args ++ sections


-- | Convert the command line arguments into 'Fields'.
fields_ :: Args -> F.Fields
fields_ args
   | fs@(_:_) <- A.only args
   = fs

   | fs@(_:_) <- A.ignore args
   = F.allFields \\ fs

   | otherwise
   = F.allFields


-- | Convert the command line arguments into 'Sections'.
sections_ :: Args -> GenericPackageDescription -> [CL.Section]
sections_ args pkgDescrp
   | A.allSections args
   = CL.allSections pkgDescrp

   | ss@(_:_) <- explicitSections args
   = ss

   | otherwise
   = CL.allSections pkgDescrp


explicitSections :: Args -> [CL.Section]
explicitSections args =
   concat [ [CL.Library | A.library args]
          , map CL.Executable (A.executable args)
          , map CL.TestSuite (A.testSuite args)
          , map CL.Benchmark (A.benchmark args)
          ]
