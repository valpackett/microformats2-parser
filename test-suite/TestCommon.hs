{-# LANGUAGE UnicodeSyntax #-}

module TestCommon where

import           Text.RawString.QQ
import           Language.Haskell.TH.Quote

xml ∷ QuasiQuoter
xml = r -- vim
