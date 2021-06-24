{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
module Main (main) where

import           Aws.Lambda
import           Aws.Lambda.Wai
import           WebApp

main ∷ IO ()
main = runWaiAsProxiedHttpLambda defaultDispatcherOptions (ignoreALBPathPart "mf2") (HandlerName "mf2") app
