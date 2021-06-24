{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main (main) where

import           Network.Wai.Cli
import           WebApp

main = defWaiMain =<< app
