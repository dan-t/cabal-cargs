{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

module CabalCargs.Fields
   ( Field(..)
   , allFields
   , Fields
   ) where

import Data.Data (Data, Typeable)

-- | A compiler relevant field. Till 'Package_Db' all fields are from the cabal file
--   with the same name, just with lower case letters and the "_" replaced by a "-".
data Field = Hs_Source_Dirs
           | Ghc_Options
           | Default_Extensions
           | Default_Language

           | Cpp_Options
           | C_Sources
           | Cc_Options

           | Extra_Lib_Dirs
           | Extra_Libraries
           | Ld_Options

           | Include_Dirs
           | Includes

           | Build_Depends

           | Package_Db             -- ^ the package database of a cabal sandbox
           | Root_Dir               -- ^ the root directory of the cabal package
           | Autogen_Hs_Source_Dirs -- ^ dirs of automatically generated haskell source files by cabal (e.g. Paths_*)
           | Autogen_Include_Dirs   -- ^ dirs of automatically generated include files by cabal
           | Autogen_Includes       -- ^ automatically generated include files by cabal (e.g. cabal_macros.h)

           | Hdevtools_Socket       -- ^ the socket file for hdevtools
           deriving (Data, Typeable, Show, Eq, Enum, Bounded)


-- | Get all known fields.
allFields :: [Field]
allFields = [ minBound .. maxBound ]


-- | Which fields should be collected
type Fields = [Field]
