
module CabalCargs.Sections
   ( Sections(..)
   , Section(..)
   ) where


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
