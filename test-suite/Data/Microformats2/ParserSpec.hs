{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.Microformats2.ParserSpec (spec) where

import           Test.Hspec
import           TestCommon
import           Data.Default
import           Data.Microformats2
import           Data.Microformats2.Parser
import           Control.Applicative

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec ∷ Spec
spec = do

  describe "parseGeo" $ do
    let parseGeo' = parseGeo . documentRoot . parseLBS

    it "parses valid h-geo" $ do
      parseGeo' [xml|<div>
        <p class="h-geo">
          <span class="p-latitude">37.33168</span>
          <span class="p-longitude">-122.03016</span>
          <span class="p-altitude">1.2345</span>
        </p>
        <p class="h-geo">
          <data class="p-latitude" value="123.45">
          <input class="p-latitude" value="678.9">
        </p>
      </div>|] `shouldBe` [ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345 }
                          , def { geoLatitude = [123.45, 678.9] } ]

    it "ignores invalid properties" $ do
      parseGeo' [xml|<p class="h-geo">
          <span class="p-latitude">HELLO WORLD!!</span>
          <span class="p-altitude">1.2345</span>
        </p>|] `shouldBe` [ def { geoAltitude = pure 1.2345 } ]

  describe "parseAdr" $ do
    let parseAdr' = parseAdr . documentRoot . parseLBS

    it "parses valid h-adr" $ do
      parseAdr' [xml|<div>
          <article class="h-adr">
            <span class="p-street-address">SA</span>
            <p class="p-extended-address">EA</p>
            <abbr class="p-post-office-box" title="PO">_</abbr>
            <span class="p-locality">L</span>
            <span class="p-region">R</span>
            <span class="p-postal-code">PC</span>
            <span class="p-country-name">C</span>
            <span class="p-label">LB</span>
            <span class="p-geo">G</span>
          </article>
        </div>|] `shouldBe` [ def { adrStreetAddress = pure "SA", adrExtendedAddress = pure "EA"
                                  , adrPostOfficeBox = pure "PO", adrLocality = pure "L"
                                  , adrRegion = pure "R", adrPostalCode = pure "PC"
                                  , adrCountryName = pure "C", adrLabel = pure "LB"
                                  , adrGeo = pure $ TextGeo "G" } ]

    it "parses p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="p-geo h-geo">
              <span class="p-latitude">37.33168</span>
              <span class="p-longitude">-122.03016</span>
              <span class="p-altitude">1.2345</span>
            </span>
          </span>
        </div>|] `shouldBe` [ def { adrGeo = [GeoGeo $ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345  }] } ]

    it "ignores nested h-geo not marked as p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="h-geo"><span class="p-altitude">1.2345</span></span>
          </span>
        </div>|] `shouldBe` [ def ]

    it "parses geo properties into p-geo" $ do
      parseAdr' [xml|<div>
          <span class="h-adr">
            <span class="p-latitude">37.33168</span>
            <span class="p-longitude">-122.03016</span>
            <span class="p-altitude">1.2345</span>
          </span>
        </div>|] `shouldBe` [ def { adrGeo = [GeoGeo $ def { geoLatitude = pure 37.33168, geoLongitude = pure (-122.03016), geoAltitude = pure 1.2345  }] } ]

  describe "parseCard" $ do
    let parseCard' = parseCard . documentRoot . parseLBS

    it "parses valid h-card" $ do
      parseCard' [xml|<div>
          <p class="h-card">
            <img class="u-photo" src="photo.png" alt="" />
            <a class="p-name u-url" href="http://example.org">Joe Bloggs</a>
            <a class="u-email" href="mailto:joebloggs@example.com">joebloggs@example.com</a>,
          </p>
        </div>|] `shouldBe` [ def { cardPhoto = pure "photo.png"
                                  , cardUrl = pure "http://example.org"
                                  , cardName = pure "Joe Bloggs"
                                  , cardEmail = pure "mailto:joebloggs@example.com" } ]

    it "parses p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-adr h-adr">
              <span class="p-street-address">17</span>
              <span class="p-locality">Reykjavik</span>
              <span class="p-country-name">Iceland</span>
            </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrStreetAddress = pure "17"
                                                                 , adrLocality = pure "Reykjavik"
                                                                 , adrCountryName = pure "Iceland" }) } ]

    it "ignores nested h-adr not marked as p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="h-adr"> <span class="p-country-name">Iceland</span> </p>
          </section>
        </div>|] `shouldBe` [ def ]

    it "parses adr and geo properties into p-adr" $ do
      parseCard' [xml|<div>
          <p class="h-card">
            <span class="p-street-address">17</span>
            <span class="p-locality">Reykjavik</span>
            <span class="p-country-name">Iceland</span>
            <span class="p-longitude">-122.03016</span>
          </p>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrStreetAddress = pure "17"
                                                                 , adrLocality = pure "Reykjavik"
                                                                 , adrCountryName = pure "Iceland"
                                                                 , adrGeo = [ GeoGeo $ def { geoLongitude = pure (-122.03016) } ] }) } ]

    it "parses p-geo into p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-geo h-geo"> <span class="p-longitude">-122.03016</span> </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = pure (AdrAdr $ def { adrGeo = [ GeoGeo $ def { geoLongitude = pure (-122.03016) } ] }) } ]

    it "ignores nested h-geo not marked as p-geo" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="h-geo"> <span class="p-longitude">-122.03016</span> </p>
          </section>
        </div>|] `shouldBe` [ def ]

    it "parses multiple things into p-adr" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-geo h-geo"> <span class="p-longitude">-122.03016</span> </p>
            <span class="p-altitude">-122.03016</span>
            <span class="p-country-name">Iceland</span>
            <p class="p-adr h-adr"> <span class="p-locality">Reykjavik</span> </p>
          </section>
        </div>|] `shouldBe` [ def { cardAdr = [
                                      (AdrAdr $ def { adrCountryName = pure "Iceland" 
                                                    , adrGeo = [ GeoGeo $ def { geoAltitude = pure (-122.03016) }
                                                               , GeoGeo $ def { geoLongitude = pure (-122.03016) } ] })
                                    , (AdrAdr $ def { adrLocality = pure "Reykjavik" }) ]} ]


    it "parses p-org" $ do
      parseCard' [xml|<div>
          <section class="h-card">
            <p class="p-org h-card"> <span class="p-name">IndieWebCamp</span> </p>
            <span class="p-org">Microformats</span>
          </section>
        </div>|] `shouldBe` [ def { cardOrg = [ TextCard "Microformats"
                                                , (CardCard $ def { cardName = pure "IndieWebCamp" }) ] }
                            , def { cardName = pure "IndieWebCamp" }]
