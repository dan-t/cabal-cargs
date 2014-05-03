{-# LANGUAGE Rank2Types, PatternGuards, TupleSections #-}

module CabalCargs.BuildInfo
   ( field
   ) where

import Distribution.PackageDescription (BuildInfo(..))
import Distribution.Compiler (CompilerFlavor(..))
import Control.Lens
import qualified CabalCargs.Fields as F
import qualified CabalLenses as CL
import Language.Haskell.Extension (Extension(..), KnownExtension(..), Language(..))


-- | A lens from a 'BuildInfo' to a list of stringified field entries of the 'BuildInfo'.
field :: F.Field -> Traversal' BuildInfo [String]
field F.Hs_Source_Dirs         = CL.hsSourceDirsL
field F.Ghc_Options            = CL.optionsL . traversed . filtered ((== GHC) . fst) . _2
field F.Default_Extensions     = oldAndDefaultExtensionsL . extsToStrings
field F.Default_Language       = CL.defaultLanguageL . langToString
field F.Cpp_Options            = CL.cppOptionsL
field F.C_Sources              = CL.cSourcesL
field F.Cc_Options             = CL.ccOptionsL
field F.Extra_Lib_Dirs         = CL.extraLibDirsL
field F.Extra_Libraries        = CL.extraLibsL
field F.Ld_Options             = CL.ldOptionsL
field F.Include_Dirs           = CL.includeDirsL
field F.Includes               = CL.includesL
field F.Build_Depends          = nopLens
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
      getter buildInfo      = oldExtensions buildInfo ++ defaultExtensions buildInfo
      setter buildInfo exts = buildInfo { defaultExtensions = exts }


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


-- | A lens that does nothing, always returns an empty
--   list and doesn't modify the given BuildInfo.
nopLens :: Lens' BuildInfo [String]
nopLens = lens (const []) const
