{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Data.Microformats2.Parser.Internal where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Foldable (asum)
import           Data.Maybe
import           Control.Applicative
import           Text.XML.Lens hiding (re)
import           Text.Regex.PCRE.Heavy
import           Safe (readMay)
import           Debug.Trace

tReadMay ∷ Read a ⇒ Text → Maybe a
tReadMay = readMay . T.unpack

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

hasOneClass ∷ Applicative f ⇒ [String] → (Element → f Element) → Element → f Element
hasOneClass ns = attributeSatisfies "class" $ \a → any (\x → T.isInfixOf (T.pack x) a) ns

hasClass ∷ Applicative f ⇒ String → (Element → f Element) → Element → f Element
hasClass n = attributeSatisfies "class" $ T.isInfixOf . T.pack $ n

valueParts ∷ Applicative f ⇒ (Element → f Element) → Element → f Element
valueParts = entire . hasOneClass ["value", "value-title"]

removeWhitespace ∷ Text → Text
removeWhitespace = gsub [re|(\s+|&nbsp;)|] (" " ∷ String) -- lol vim |||||||

_Content' ∷ Prism' Node Text
_Content' = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  _ → Nothing

_ContentWithAlts ∷ Prism' Node Text
_ContentWithAlts = prism' NodeContent $ \s → case s of
  NodeContent c → Just $ removeWhitespace c
  NodeElement e → if' (e ^. name == "img") $ e ^. attribute "alt"
  _ → Nothing

basicText ∷ Applicative f ⇒ (Text → f Text) → Element → f Element
basicText = entire . nodes . traverse . _Content'

getText ∷ Element → Maybe Text
getText e = Just . T.strip <$> T.concat $ e ^.. basicText

allText ∷ Applicative f ⇒ (Text → f Text) → Element → f Element
allText = entire . nodes . traverse . _ContentWithAlts

getAllText ∷ Element → Maybe Text
getAllText e = Just . T.strip <$> T.concat $ e ^.. allText

getAbbrTitle ∷ Element → Maybe Text
getAbbrTitle e = if' (e ^. name == "abbr") $ e ^. attribute "title"

getDataInputValue ∷ Element → Maybe Text
getDataInputValue e = if' (e ^. name == "data" || e ^. name == "input") $ e ^. attribute "value"

getImgSrc ∷ Element → Maybe Text
getImgSrc e = if' (e ^. name == "img") $ e ^. attribute "src"

getObjectData ∷ Element → Maybe Text
getObjectData e = if' (e ^. name == "object") $ e ^. attribute "data"

getImgAreaAlt ∷ Element → Maybe Text
getImgAreaAlt e = if' (e ^. name == "img" || e ^. name == "area") $ e ^. attribute "alt"

getAAreaHref ∷ Element → Maybe Text
getAAreaHref e = if' (e ^. name == "a" || e ^. name == "area") $ e ^. attribute "href"

getOnlyChildren ∷ Element → [Element]
getOnlyChildren e = if ((lengthOf plate e) == 1) then e ^.. plate else []

getOnlyChild ∷ Name → Element → Maybe Element
getOnlyChild n e = if' ((lengthOf plate e) == 1) $ e ^? plate . el n

getOnlyChildImgAreaAlt ∷ Element → Maybe Text
getOnlyChildImgAreaAlt e = (^. attribute "alt") =<< (asum $ getOnlyChild <$> [ "img", "area" ] <*> pure e)

getOnlyChildAbbrTitle ∷ Element → Maybe Text
getOnlyChildAbbrTitle e = (^. attribute "title") =<< getOnlyChild "abbr" e

getOnlyOfType ∷ Name → Element → Maybe Element
getOnlyOfType n e = if' ((lengthOf (plate . el n) e) == 1) $ e ^? plate . el n

getOnlyOfTypeImgSrc ∷ Element → Maybe Text
getOnlyOfTypeImgSrc e = (^. attribute "src") =<< getOnlyOfType "img" e

getOnlyOfTypeObjectData ∷ Element → Maybe Text
getOnlyOfTypeObjectData e = (^. attribute "data") =<< getOnlyOfType "object" e

getOnlyOfTypeAAreaHref ∷ Element → Maybe Text
getOnlyOfTypeAAreaHref e = (^. attribute "href") =<< (asum $ getOnlyOfType <$> [ "a", "area" ] <*> pure e)

extractValue ∷ Element → Maybe Text
extractValue e = asum $ [ getAbbrTitle, getDataInputValue, getImgAreaAlt, getAllText ] <*> pure e

extractValueClassPattern ∷ Element → Maybe Text
extractValueClassPattern e' = if' (isJust $ e' ^? valueParts) $ extractValueParts e'
  where extractValueParts e = Just . T.concat . catMaybes $ e ^.. valueParts . to extractValuePart
        extractValuePart  e = asum $ [ extractValueTitle, extractValue ] <*> pure e
        extractValueTitle e = if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"

findProperty ∷ Element → String → Maybe Element
findProperty e n = e ^? entire . hasClass n

data PropType = P | U | Dt | E

extractProperty ∷ PropType → String → Element → Maybe Text
extractProperty P n e' = do
  e ← findProperty e' $ "p-" ++ n
  asum $ [ extractValueClassPattern, extractValue ] <*> pure e

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
