{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}

module Data.Microformats2.Parser.InternalSpec (spec) where

import           Test.Hspec
import           Text.RawString.QQ
import           Text.HTML.DOM
import           Text.XML.Lens (documentRoot)
import           Data.Microformats2.Parser.Internal

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec ∷ Spec
spec = do
  describe "extractProperty" $ do
    it "parses p- properties" $ do
      let nm = extractProperty P "name" . documentRoot . parseLBS
      nm [r|<span class="p-name">Hello Basic</span>|] `shouldBe` Just "Hello Basic"
      nm [r|<abbr class="p-name" title="Hello Abbr">HA</abbr>|] `shouldBe` Just "Hello Abbr"
      nm [r|<abbr class="p-name">HA</abbr>|] `shouldBe` Just "HA"
      nm [r|<data class="p-name" value="Hello Data" />|] `shouldBe` Just "Hello Data"
      nm [r|<input class="p-name" value="Hello Input" />|] `shouldBe` Just "Hello Input"
      nm [r|<img class="p-name" alt="Hello Img" />|] `shouldBe` Just "Hello Img"
      nm [r|<area class="p-name" alt="Hello Area" />|] `shouldBe` Just "Hello Area"
      nm [r|<span class="p-name"> ignore <i class="value">Hello</i>  <img class="value" alt="ValuePattern" src="x.png"> </span>|] `shouldBe` Just "HelloValuePattern"
      nm [r|<span class="p-name"> ignore <em class="value-title" title="Hello">Hi</em>  <span class="value">Value-Title</span></span>|] `shouldBe` Just "HelloValue-Title"
      nm [r|<span class="p-name">  Hello <img alt="Span With Img" src="x.png"> </span>|] `shouldBe` Just "Hello Span With Img"
      nm [r|<span class="p-name"> <span class="value"> 	Hello 	
  <img alt="Span With Img" src="x.png"> </span> 	<em class="value-title" title="&& Value Title">nope</em> </span>|] `shouldBe` Just "Hello Span With Img&& Value Title"
