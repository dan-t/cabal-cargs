{-# LANGUAGE DeriveDataTypeable #-}

module CabalCargs.Field
   ( Field(..)
   ) where

import Data.Data (Data, Typeable)

-- | A compiler relevant field from the cabal file.
data Field = Hs_Source_Dirs
           | Ghc_Options
           | Extensions

           | Cpp_Options
           | C_Sources
           | CC_Options

           | Extra_Lib_Dirs
           | Extra_Libraries
           | Ld_Options

           | Include_Dirs
           | Includes

           -- | This isn't a field of the cabal file, but represents
           --   the package database of a cabal sandbox.
           | Package_Db
           deriving (Data, Typeable, Show, Eq)
