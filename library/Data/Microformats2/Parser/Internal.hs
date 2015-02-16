{-# LANGUAGE OverloadedStrings, QuasiQuotes, UnicodeSyntax #-}

module Data.Microformats2.Parser.Internal where

-- import           Data.Microformats2
import           Data.Text as T
import           Data.Foldable (asum)
import           Data.Maybe (isJust, catMaybes)
import           Control.Applicative ((<$>), Applicative)
import           Text.XML.Lens hiding (re)
import           Text.Regex.PCRE.Heavy
-- import           Safe (headMay)

documentRoot ∷ Document → Element
documentRoot = view root

if' ∷ Bool → Maybe a → Maybe a
if' c x = if c then x else Nothing

hasOneClass ∷ Applicative f => [String] → (Element → f Element) → Element → f Element
hasOneClass ns = attributeSatisfies "class" $ \a -> Prelude.any (\x -> T.isInfixOf (T.pack x) a) ns

hasClass ∷ Applicative f => String → (Element → f Element) → Element → f Element
hasClass n = attributeSatisfies "class" $ T.isInfixOf . T.pack $ n

valueParts ∷ Applicative f => (Element → f Element) → Element → f Element
valueParts = entire . hasOneClass ["value", "value-title"]

_Content' ∷ Prism' Node Text
_Content' = prism' NodeContent $ \s -> case s of
  NodeContent c -> Just $ gsub [re|\s+|] (" " :: String) c
  NodeElement e -> if' (e ^. name == "img") $ e ^. attribute "alt"
  _ -> Nothing

allText ∷ Applicative f => (Text → f Text) → Element → f Element
allText = entire . nodes . traverse . _Content'

extractValue ∷ Element → Maybe T.Text
extractValue e =
  asum  [ if' (e ^. name == "abbr") $ e ^. attribute "title"
        , if' (e ^. name == "data" || e ^. name == "input") $ e ^. attribute "value"
        , if' (e ^. name == "img"  || e ^. name == "area")  $ e ^. attribute "alt"
        , if' True $ Just $ T.strip <$> T.concat $ e ^.. allText ]

extractValuePart ∷ Element → Maybe T.Text
extractValuePart e =
  asum  [ if' (isJust $ e ^? hasClass "value-title") $ e ^. attribute "title"
        , extractValue e ]

findProperty ∷ Element → String → Maybe Element
findProperty e n = e ^? entire . hasClass n

data PropType = P | U | Dt | E

extractProperty ∷ PropType → String → Element → Maybe T.Text
extractProperty P n e' = do
  e <- findProperty e' $ "p-" ++ n
  asum  [ if' (isJust $ e ^? valueParts) $ Just $ T.concat $ catMaybes $ e ^.. valueParts . to extractValuePart
        , extractValue e ]
