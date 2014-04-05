
module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), (<.>))
import CabalCargs.Args
import CabalCargs.Formatting
import qualified CabalCargs.Fields as F
import qualified CabalCargs.Format as Fmt
import qualified CabalCargs.CompilerArgs as CompilerArgs
import Data.List (intercalate)


main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "Tests" [withoutSandboxTests, withSandboxTests]

withoutSandboxTests :: T.TestTree
withoutSandboxTests = testsWithBaseDir "withoutSandbox"

withSandboxTests :: T.TestTree
withSandboxTests = testsWithBaseDir "withSandbox"

testsWithBaseDir :: FilePath -> T.TestTree
testsWithBaseDir dir = T.testGroup dir
   [ test dir "FindCabalFile" $ defaultArgs { sourceFile = libDir }
   , test dir "FromCabalFile" $ defaultArgs { cabalFile = cabalFile } 
   , test dir "FromLibSrcFile" $ defaultArgs { sourceFile = libSrcFile }
   , test dir "FromExeSrcFile" $ defaultArgs { sourceFile = exeSrcFile }

   , test dir "FindCabalFileHdevtools" $ defaultArgs { sourceFile = libDir, format = Hdevtools }
   , test dir "FromCabalFileHdevtools" $ defaultArgs { sourceFile = cabalFile, format = Hdevtools }
   , test dir "FromLibSrcFileHdevtools" $ defaultArgs { sourceFile = libSrcFile, format = Hdevtools }
   , test dir "FromExeSrcFileHdevtools" $ defaultArgs { sourceFile = exeSrcFile, format = Hdevtools }

   , test dir "FindCabalFilePure" $ defaultArgs { sourceFile = libDir, format = Pure }
   , test dir "FromCabalFilePure" $ defaultArgs { sourceFile = cabalFile, format = Pure }
   , test dir "FromLibSrcPure" $ defaultArgs { sourceFile = libSrcFile, format = Pure }
   , test dir "FromExeSrcFilePure" $ defaultArgs { sourceFile = exeSrcFile, format = Pure }

   , test dir "AllOfLib" $ defaultArgs { library = True, cabalFile = cabalFile }
   , test dir "AllOfExe" $ defaultArgs { executable = ["cabal-cargs"], cabalFile = cabalFile }
   , test dir "AllOfTest" $ defaultArgs { testSuite = ["tests"], cabalFile = cabalFile }

   , test dir "OnlyGhcOptionsOfLib" $ defaultArgs { library = True, cabalFile = cabalFile, only = [F.Ghc_Options] }
   , test dir "OnlyGhcOptionsOfExe" $ defaultArgs { executable = ["cabal-cargs"], cabalFile = cabalFile, only = [F.Ghc_Options] }
   , test dir "OnlyGhcOptionsOfTest" $ defaultArgs { testSuite = ["tests"], cabalFile = cabalFile, only = [F.Ghc_Options] }

   , test dir "OnlyPureSrcDirsOfLib" $ defaultArgs { library = True, cabalFile = cabalFile, only = [F.Hs_Source_Dirs], format = Pure }
   , test dir "OnlyPureSrcDirsOfExe" $ defaultArgs { executable = ["cabal-cargs"], cabalFile = cabalFile, only = [F.Hs_Source_Dirs], format = Pure }
   , test dir "OnlyPureSrcDirsOfTest" $ defaultArgs { testSuite = ["tests"], cabalFile = cabalFile, only = [F.Hs_Source_Dirs], format = Pure }

   , test dir "EnableFlag" $ defaultArgs { cabalFile = cabalFile, enable = ["default_false_flag"] }
   , test dir "DisableFlag" $ defaultArgs { cabalFile = cabalFile, disable = ["default_true_flag"] }
   , test dir "EnableAndDisableFlag" $ defaultArgs { cabalFile = cabalFile, enable = ["default_false_flag"], disable = ["default_true_flag"] }

   , test dir "IgnoreBuildDepends" $ defaultArgs { cabalFile = cabalFile, ignore = [F.Build_Depends] }
   , test dir "AllSections" $ defaultArgs { sourceFile = libSrcFile, allSections = True }
   ]

   where
      cabalFile  = Just $ inputDir </> "test.cabal"
      libSrcFile = Just $ inputDir </> "lib" </> "Source.hs"
      exeSrcFile = Just $ inputDir </> "exe" </> "Source.hs"
      libDir     = Just $ inputDir </> "lib"
      inputDir   = "tests" </> "inputFiles" </> dir


test :: FilePath -> String -> Args -> T.TestTree
test dir testName args =
   G.goldenVsFileDiff (testName ++ " " ++ dir) diff goldenFile outputFile command
   where
      command = do
         let formatting = format args
         cargs <- CompilerArgs.fromCmdArgs args
         case cargs of
              Left error -> do
                 hPutStrLn stderr ("cabal-cargs: " ++ error)

              Right cargs_ -> do
                 writeFile outputFile (intercalate " " $ Fmt.format formatting cargs_)

      diff ref new = ["diff", "-u", ref, new]
      goldenFile   = "tests" </> "goldenFiles" </> dir </> testName <.> "txt"
      outputFile   = "tests" </> "outputFiles" </> dir </> testName <.> "txt"


defaultArgs :: Args
defaultArgs = Args
   { library     = False
   , executable  = []
   , testSuite   = []
   , benchmark   = []
   , allSections = False
   , only        = []
   , ignore      = []
   , format      = Ghc
   , sourceFile  = Nothing
   , cabalFile   = Nothing
   , enable      = []
   , disable     = []
   , os          = Nothing
   , arch        = Nothing
   , relative    = True
   }
