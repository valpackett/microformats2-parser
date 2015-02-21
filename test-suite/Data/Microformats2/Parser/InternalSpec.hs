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

  describe "implyProperty" $ do
    it "parses implied p-name" $ do
      let nm = implyProperty P "name" . documentRoot . parseLBS
      nm [r|<img class="h-blah" alt="Hello Img!">|] `shouldBe` Just "Hello Img!"
      nm [r|<abbr class="h-blah" title="Hello Abbr!">HA</abbr>|] `shouldBe` Just "Hello Abbr!"
      nm [r|<p class="h-blah"><img alt="Hello Only Img!"></p>|] `shouldBe` Just "Hello Only Img!"
      nm [r|<p class="h-blah"><img alt="DOING"><img alt="IT"><img alt="WRONG">Goodbye Img!</p>|] `shouldBe` Just "Goodbye Img!"
      nm [r|<p class="h-blah"><abbr title="Hello Only Abbr!">HOA</abbr></p>|] `shouldBe` Just "Hello Only Abbr!"
      nm [r|<p class="h-blah"><abbr title="DOING"/><abbr title="IT"/><abbr title="WRONG"/>Goodbye Abbr!</p>|] `shouldBe` Just "Goodbye Abbr!"
      nm [r|<p class="h-blah"><em><img alt="Hello Only Nested Img!"></p>|] `shouldBe` Just "Hello Only Nested Img!"
      nm [r|<p class="h-blah"><em><img alt="DOING"><img alt="WRONG">Goodbye Nested Img!</p>|] `shouldBe` Just "Goodbye Nested Img!"
      nm [r|<p class="h-blah"><em><abbr title="Hello Only Nested Abbr!">HOA</abbr></p>|] `shouldBe` Just "Hello Only Nested Abbr!"
      nm [r|<p class="h-blah"><em><abbr title="DOING"/><abbr title="WRONG"/>Goodbye Nested Abbr!</p>|] `shouldBe` Just "Goodbye Nested Abbr!"
      nm [r|<p class="h-blah">Hello Text!</p>|] `shouldBe` Just "Hello Text!"

    it "parses implied u-photo" $ do
      let ph = implyProperty U "photo" . documentRoot . parseLBS
      ph [r|<img class="h-blah" src="selfie.png">|] `shouldBe` Just "selfie.png"
      ph [r|<object class="h-blah" data="art.svg">|] `shouldBe` Just "art.svg"
      ph [r|<div class="h-blah"><img src="selfie.png"/><em>yo</em>|] `shouldBe` Just "selfie.png"
      ph [r|<div class="h-blah"><object data="art.svg"/><em>yo</em>|] `shouldBe` Just "art.svg"
      ph [r|<div class="h-blah"><p class="onlychild"><img src="selfie.png"/><em>yo</em>|] `shouldBe` Just "selfie.png"
      ph [r|<div class="h-blah"><p class="onlychild"><object data="art.svg"/><em>yo</em>|] `shouldBe` Just "art.svg"

    it "parses implied u-url" $ do
      let ur = implyProperty U "url" . documentRoot . parseLBS
      ur [r|<a class="h-blah" href="/hello/a">|] `shouldBe` Just "/hello/a"
      ur [r|<area class="h-blah" href="/hello/area">|] `shouldBe` Just "/hello/area"
      ur [r|<div class="h-blah"><em>what</em><a href="/hello/n/a">|] `shouldBe` Just "/hello/n/a"
      ur [r|<div class="h-blah"><em>what</em><area href="/hello/n/area">|] `shouldBe` Just "/hello/n/area"
