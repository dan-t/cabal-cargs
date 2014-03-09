{-# LANGUAGE PatternGuards #-}

module CabalCargs.Sections
   ( Sections(..)
   , Section(..)
   , sections
   ) where

import CabalCargs.Args (Args)
import qualified CabalCargs.Args as A

-- | A section of the cabal file.
data Section = Library
             | Executable String
             | TestSuite String
             | Benchmark String
             deriving (Show, Eq)

-- | From which sections the compiler args should be collected.
data Sections = AllSections        -- ^ all sections are considered
              | Sections [Section] -- ^ only these sections are considered
              deriving (Show, Eq)


-- | Convert the command line arguments into 'Sections'.
sections :: Args -> Sections
sections args 
   | ss@(_:_) <- concat [ if (A.library args) then [Library] else []
                        , map Executable (A.executable args)
                        , map TestSuite (A.testSuite args)
                        , map Benchmark (A.benchmark args)
                        ]
   = Sections ss

   | otherwise
   = AllSections
