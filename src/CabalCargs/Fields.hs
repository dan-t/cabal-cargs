{-# LANGUAGE PatternGuards #-}

module CabalCargs.Fields
   ( Fields(..)
   , fields
   ) where

import CabalCargs.Args (Args)
import qualified CabalCargs.Args as Args
import qualified CabalCargs.Field as F


-- | Which fields should be considered for the print out.
data Fields = AllFields        -- ^ all fields are printed out
            | Fields [F.Field] -- ^ only these fields are printed out
            deriving (Show, Eq)


-- | Convert the command line arguments into 'Fields'.
fields :: Args -> Fields
fields args
   | fs@(_:_) <- Args.only args
   = Fields fs

   | otherwise
   = AllFields
