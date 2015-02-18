{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
import           Text.RawString.QQ
import           Data.Default
import           Data.Microformats2
import           Data.Microformats2.Parser

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec ∷ Spec
spec = do

  describe "parseGeo" $ do
    let parseGeo' = parseGeo . documentRoot . parseLBS

    it "parses valid h-geo" $ do
      parseGeo' [r|<div>
        <p class="h-geo">
          <span class="p-latitude">37.33168</span>
          <span class="p-longitude">-122.03016</span>
          <span class="p-altitude">1.2345</span>
        </p>
        <p class="h-geo">
          <data class="p-latitude" value="1.2345">
        </p>
      </div>|] `shouldBe` [ def { geoLatitude = Just 37.33168, geoLongitude = Just (-122.03016), geoAltitude = Just 1.2345 }
                          , def { geoLatitude = Just 1.2345 } ]

    it "ignores invalid properties" $ do
      parseGeo' [r|<p class="h-geo">
          <span class="p-latitude">HELLO WORLD!!</span>
          <span class="p-altitude">1.2345</span>
        </p>|] `shouldBe` [ def { geoAltitude = Just 1.2345 } ]
