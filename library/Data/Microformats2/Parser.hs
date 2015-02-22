{-# LANGUAGE UnicodeSyntax #-}

module Data.Microformats2.Parser (
  parseGeo
, parseAdr

  -- * HTML parsing stuff (from html-conduit, xml-lens)
, documentRoot
, parseLBS
, sinkDoc
) where

-- import qualified Data.Text as T
import           Data.Microformats2
import           Data.Microformats2.Parser.Internal
import           Data.Default
import           Text.HTML.DOM
import           Text.XML.Lens

mapC ∷ String → (Element → α) → Element → [α]
mapC n p e = map p $ e ^.. entire . hasClass n

-- | Parses all h-geo entries inside of an element.
parseGeo ∷ Element → [Geo]
parseGeo = mapC "h-geo" parse
  where parse e = def { geoLatitude  = extractPropertyR P "latitude"  e
                      , geoLongitude = extractPropertyR P "longitude" e
                      , geoAltitude  = extractPropertyR P "altitude"  e }

-- | Parses all h-adr entries inside of an element.
parseAdr ∷ Element → [Adr]
parseAdr = mapC "h-adr" parse
  where parse e = def { adrStreetAddress   = extractPropertyL P "street-address"   e
                      , adrExtendedAddress = extractPropertyL P "extended-address" e
                      , adrPostOfficeBox   = extractPropertyL P "post-office-box"  e
                      , adrLocality        = extractPropertyL P "locality"         e
                      , adrRegion          = extractPropertyL P "region"           e
                      , adrPostalCode      = extractPropertyL P "postal-code"      e
                      , adrCountryName     = extractPropertyL P "country-name"     e
                      , adrLabel           = extractPropertyL P "label"            e
                      }
