{-# LANGUAGE UnicodeSyntax #-}

module Data.Microformats2.Parser (
  parseGeo

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

-- | Parses all h-geo entries inside of an element
parseGeo ∷ Element → [Geo]
parseGeo e = map parse $ e ^.. entire . hasClass "h-geo"
  where parse ee = def { geoLatitude  = tReadMay =<< extractProperty P "latitude"  ee
                       , geoLongitude = tReadMay =<< extractProperty P "longitude" ee
                       , geoAltitude  = tReadMay =<< extractProperty P "altitude"  ee }
