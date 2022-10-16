{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, OverloadedStrings, CPP #-}

module Data.Microformats2.Jf2 (mf2ToJf2) where

import           Prelude.Compat
import           Control.Lens
import           Data.Maybe
import           Data.Aeson.Lens
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
#else
import qualified Data.HashMap.Strict as KM
#endif
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Microformats2.Parser.UnsafeUtil

mf2ToJf2 ∷ Value → Value
mf2ToJf2 val@(Object _) = flattenItems $ processItems $ processChildren $ flattenProps $ val & key "type" %~ flattenType
  where flattenType t@(Array _) = fromMaybe Null $ fmap (& _String %~ (\x → fromMaybe x $ T.stripPrefix "h-" x)) $ t ^? nth 0
        flattenType x = x
        flattenProps p@(Object o) = Object $ KM.delete "value" $ KM.delete "properties" $
          foldl (\acc (k, v) → KM.insert k (flattenArr $ processProp k v) acc) o $
#if MIN_VERSION_aeson(2,0,0)
          map (\(k, v) -> (K.fromText k, v)) $
#endif
          p ^@.. key "properties" . members
        flattenProps x = x
        processProp "content" v = v & _Array . each %~ processContent
        processProp _ v = v & _Array . each %~ mf2ToJf2
        processContent (Object o) =
          case KM.lookup "value" o of
               Just v → Object $ KM.delete "value" $ KM.insert "text" v o
               _ → Object o
        processContent x = x
        flattenArr (Array v) | V.length v == 1 = V.head v
        flattenArr x = x
        processChildren x = x & key "children" . _Array . each %~ mf2ToJf2
        processItems x = x & key "items" . _Array . each %~ mf2ToJf2
        flattenItems x | x ^? key "items" . _Array . to V.length == Just 1 = fromMaybe x $ x ^? key "items" . nth 0
        flattenItems x = x
mf2ToJf2 x = x & _Array . each %~ mf2ToJf2
