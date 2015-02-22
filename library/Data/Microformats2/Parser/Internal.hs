{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}

module Data.Microformats2.Parser.Internal where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Maybe
import           Control.Applicative
import           Text.XML.Lens hiding (re)
import           Text.Blaze
import           Text.Blaze.Renderer.Text
import           Text.Regex.PCRE.Heavy
import           Safe (readMay)

tReadMay ∷ Read a ⇒ Text → Maybe a
tReadMay = readMay . T.unpack

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

hasOneClass ∷ Applicative f ⇒ [String] → (Element → f Element) → Element → f Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → T.isInfixOf (T.pack x) a) ns

hasClass ∷ Applicative f ⇒ String → (Element → f Element) → Element → f Element
hasClass n = attributeSatisfies "class" $ T.isInfixOf . T.pack $ n

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if ((lengthOf plate e) == 1) then e ^.. plate else []

getOnlyChild ∷ Name → Element → Maybe Element
getOnlyChild n e = if' ((lengthOf plate e) == 1) $ e ^? plate . el n

getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyOfType n e = if' ((lengthOf (plate . el n) e) == 1) $ e ^? plate . el n

els ∷ [Name] → Traversal' Element Element
els ns f s = if (elementName s) `elem` ns then f s else pure s

removeWhitespace ∷ Text → Text
removeWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String) -- lol vim |||||||

_Content' ∷ Prism' Node Text
_Content' = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  _ → Nothing

_InnerHtml ∷ Prism' Node Text
_InnerHtml = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → Just . TL.toStrict . renderMarkup . toMarkup $ e
  _ → Nothing

_ContentWithAlts ∷ Prism' Node Text
_ContentWithAlts = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → e ^. el "img" . attribute "alt"
  _ → Nothing

basicText ∷ Applicative f ⇒ (Text → f Text) → Element → f Element
basicText = entire . nodes . traverse . _Content'

getText ∷ Element → Maybe Text
getText e = Just . T.strip <$> T.concat $ e ^.. basicText

getAllText ∷ Element → Maybe Text
getAllText e = Just . T.strip <$> T.concat $ e ^.. entire . nodes . traverse . _ContentWithAlts

getAllHtml ∷ Element → Maybe Text
getAllHtml e = Just . T.strip <$> T.concat $ e ^.. nodes . traverse . _InnerHtml

getAbbrTitle ∷ Element → Maybe Text
getAbbrTitle e = e ^. el "abbr" . attribute "title"

getDataInputValue ∷ Element → Maybe Text
getDataInputValue e = e ^. els ["data", "input"] . attribute "value"

getImgSrc ∷ Element → Maybe Text
getImgSrc e = e ^. el "img" . attribute "src"

getObjectData ∷ Element → Maybe Text
getObjectData e = e ^. el "object" . attribute "data"

getImgAreaAlt ∷ Element → Maybe Text
getImgAreaAlt e = e ^. els ["img", "area"] . attribute "alt"

getAAreaHref ∷ Element → Maybe Text
getAAreaHref e = e ^. els ["a", "area"] . attribute "href"

getImgAudioVideoSourceSrc ∷ Element → Maybe Text
getImgAudioVideoSourceSrc e = e ^. els ["img", "audio", "video", "source"] . attribute "src"

getTimeInsDelDatetime ∷ Element → Maybe Text
getTimeInsDelDatetime e = e ^. els ["time", "ins", "del"] . attribute "datetime"

getOnlyChildImgAreaAlt ∷ Element → Maybe Text
getOnlyChildImgAreaAlt e = (^. attribute "alt") =<< (asum $ getOnlyChild <$> [ "img", "area" ] <*> pure e)

getOnlyChildAbbrTitle ∷ Element → Maybe Text
getOnlyChildAbbrTitle e = (^. attribute "title") =<< getOnlyChild "abbr" e

getOnlyOfTypeImgSrc ∷ Element → Maybe Text
getOnlyOfTypeImgSrc e = (^. attribute "src") =<< getOnlyOfType "img" e

getOnlyOfTypeObjectData ∷ Element → Maybe Text
getOnlyOfTypeObjectData e = (^. attribute "data") =<< getOnlyOfType "object" e

getOnlyOfTypeAAreaHref ∷ Element → Maybe Text
getOnlyOfTypeAAreaHref e = (^. attribute "href") =<< (asum $ getOnlyOfType <$> [ "a", "area" ] <*> pure e)

extractValue ∷ Element → Maybe Text
extractValue e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getAllText ] <*> pure e

extractValueTitle ∷ Element → Maybe Text
extractValueTitle e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

extractValueClassPattern ∷ [Element → Maybe Text] → Element → Maybe Text
extractValueClassPattern fs e = if' (isJust $ e ^? valueParts) $ extractValueParts
  where extractValueParts   = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart e' = asum $ fs <*> pure e'
        valueParts          ∷ Applicative f => (Element → f Element) → Element → f Element
        valueParts          = entire . hasOneClass ["value", "value-title"]

findProperty ∷ Element → String → Maybe Element
findProperty e n = e ^? entire . hasClass n

data PropType = P | U | Dt | E

extractProperty ∷ PropType → String → Element → Maybe Text
extractProperty P n e' = do
  e ← findProperty e' $ "p-" ++ n
  asum $ [ extractValueClassPattern [extractValueTitle, extractValue]
         , extractValue ] <*> pure e
extractProperty U n e' = do
  e ← findProperty e' $ "u-" ++ n
  asum $ [ getAAreaHref, getImgAudioVideoSourceSrc
         , extractValueClassPattern [extractValueTitle, extractValue]
         , getAbbrTitle, getDataInputValue, getAllText ] <*> pure e
extractProperty Dt n e' = do
  e ← findProperty e' $ "dt-" ++ n
  let ms = [ getTimeInsDelDatetime, getAbbrTitle, getDataInputValue ]
  asum $ (extractValueClassPattern ms) : ms ++ [getAllText] <*> pure e
extractProperty E n e' = findProperty e' ("e-" ++ n) >>= getAllHtml

implyProperty ∷ PropType → String → Element → Maybe Text
implyProperty P "name"  e = asum $ [ getImgAreaAlt, getAbbrTitle
                                   , getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle
                                   , \e' -> asum $ [ getOnlyChildImgAreaAlt, getOnlyChildAbbrTitle ] <*> getOnlyChildren e'
                                   , getText ] <*> pure e
implyProperty U "photo" e = asum $ [ getImgSrc, getObjectData
                                   , getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData
                                   , \e' -> asum $ [ getOnlyOfTypeImgSrc, getOnlyOfTypeObjectData ] <*> getOnlyChildren e'
                                   ] <*> pure e
implyProperty U "url"   e = asum $ [ getAAreaHref, getOnlyOfTypeAAreaHref ] <*> pure e
implyProperty _ _ _ = Nothing
