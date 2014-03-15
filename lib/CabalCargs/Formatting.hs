{-# LANGUAGE DeriveDataTypeable #-}

module CabalCargs.Formatting
   ( Formatting(..)
   ) where

import Data.Data (Data, Typeable)
import System.Console.CmdArgs.Default (Default, def)


-- | How the fields from the cabal file should be printed out.
data Formatting = Ghc       -- ^ as ghc compatible arguments
                | Hdevtools -- ^ as hdevtools compatible arguments
                | Pure      -- ^ the field values are printed as present in the cabal file
                deriving (Data, Typeable, Show, Eq)


instance Default Formatting where
   def = Ghc
