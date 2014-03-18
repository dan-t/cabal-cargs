{-# Language TemplateHaskell, PatternGuards #-}

module CabalCargs.CondVars
   ( CondVars(..)
   , fromDefaults
   , enableFlag
   , disableFlag
   , eval
   ) where

import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription (Condition(..))
import qualified Distribution.System as S
import Distribution.System (OS(..), Arch(..))
import qualified Data.HashMap.Strict as HM
import Control.Lens

type FlagName = String
type FlagMap  = HM.HashMap FlagName Bool


-- | The conditional variables that are used to resolve the conditionals inside of
--   the cabal file. Holds the enable state of the cabal flags and the used OS and ARCH.
data CondVars = CondVars
   { flags :: FlagMap  -- ^ initialized with the default flag values which are accordingly
                       --   modified by the explicitely given flag values by the user
   , os    :: OS       -- ^ the used OS, by default the one cabal was build on or given explicitely by the user
   , arch  :: Arch     -- ^ the used ARCH, by default the one cabal was build on or given explicitely by the user
   } deriving (Show)


makeLensesFor [ ("flags", "flagsL")
              ] ''CondVars


-- | Create a 'CondVars' from the default flags of the cabal package description.
--   The 'os' and 'arch' fields are initialized by the ones the cabal library was build on.
fromDefaults :: PD.GenericPackageDescription -> CondVars
fromDefaults pkgDescrp = CondVars { flags = flags, os = S.buildOS, arch = S.buildArch }
   where
      flags = HM.fromList $ map nameWithDflt (PD.genPackageFlags pkgDescrp)

      nameWithDflt PD.MkFlag { PD.flagName = PD.FlagName name, PD.flagDefault = dflt } =
         (name, dflt)


-- | Enable the given flag in 'CondVars'.
enableFlag :: FlagName -> CondVars -> CondVars
enableFlag flag condVars =
   condVars & flagsL %~ (HM.insert flag True)


-- | Disable the given flag in 'CondVars'.
disableFlag :: FlagName -> CondVars -> CondVars
disableFlag flag condVars =
   condVars & flagsL %~ (HM.insert flag False)


-- | Evaluate the 'Condition' using the 'CondVars'.
eval :: CondVars -> (Condition PD.ConfVar) -> Bool
eval condVars cond = eval' cond
   where
      eval' (Var var)    = hasVar var
      eval' (Lit val)    = val
      eval' (CNot c)     = not $ eval' c
      eval' (COr c1 c2)  = eval' c1 || eval' c2
      eval' (CAnd c1 c2) = eval' c1 && eval' c2

      hasVar (PD.OS osVar)                = osVar == os condVars
      hasVar (PD.Arch archVar)            = archVar == arch condVars
      hasVar (PD.Impl _ _ )               = True
      hasVar (PD.Flag (PD.FlagName name))
         | Just v <- HM.lookup name (flags condVars)
         = v

         | otherwise
         = False
