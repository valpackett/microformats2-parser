{-# LANGUAGE Trustworthy, NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, QuasiQuotes, CPP #-}

module Data.Microformats2.Parser.UnsafeUtil (
  module Data.Microformats2.Parser.UnsafeUtil
, module X
) where

import           Prelude.Compat
import           Control.Lens as X hiding (re, (.=))
import           Data.Aeson
import           Data.Aeson.Types as X
import           Data.Default as X
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import           Text.Regex.PCRE.Heavy
import           Text.XML.Lens as X
import           Text.HTML.DOM as X (sinkDoc, parseLBS)
import           Text.Blaze
import           Text.Blaze.Renderer.Text

collapseWhitespace ∷ T.Text → T.Text
collapseWhitespace = gsub [re|(\s|&nbsp;)+|] (" " ∷ String)

emptyVal ∷ Value → Bool
emptyVal (Object o) = KM.null o
emptyVal (Array v) = V.null v
emptyVal (String s) = T.null s
emptyVal Null = True
emptyVal _ = False

renderInner ∷ Element → T.Text
renderInner = T.concat . map renderNode . elementNodes
  where renderNode (NodeContent c) = c
        renderNode (NodeElement e) = TL.toStrict $ renderMarkup $ toMarkup e
        renderNode _ = ""

vsingleton ∷ Maybe T.Text → Value
vsingleton x = fromMaybe Null $ (Array . V.singleton . String) <$> x

extractVector ∷ Value → Array
extractVector (Array v) = v
extractVector _ = V.empty

mergeProps ∷ (τ, [(α, Value)]) → (τ, Value)
mergeProps (n, vs) = (n, Array $ V.concat $ reverse $ map (extractVector . snd) vs)
