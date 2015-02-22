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

    it "parses u- properties" $ do
      let ur = extractProperty U "url" . documentRoot . parseLBS
      ur [r|<a class="u-url" href="/yo/a">link</a>|] `shouldBe` Just "/yo/a"
      ur [r|<area class="u-url" href="/yo/area"/>|] `shouldBe` Just "/yo/area"
      ur [r|<img class="u-url" src="/yo/img"/>|] `shouldBe` Just "/yo/img"
      ur [r|<audio class="u-url" src="/yo/audio"/>|] `shouldBe` Just "/yo/audio"
      ur [r|<video class="u-url" src="/yo/video"/>|] `shouldBe` Just "/yo/video"
      ur [r|<source class="u-url" src="/yo/source"/>|] `shouldBe` Just "/yo/source"
      ur [r|<span class="u-url"><b class=value>/yo</b><em class="value">/vcp</span>|] `shouldBe` Just "/yo/vcp"
      ur [r|<abbr class="u-url" title="/yo/abbr"/>|] `shouldBe` Just "/yo/abbr"
      ur [r|<data class="u-url" value="/yo/data"/>|] `shouldBe` Just "/yo/data"
      ur [r|<input class="u-url" value="/yo/input"/>|] `shouldBe` Just "/yo/input"
      ur [r|<span class="u-url">/yo/span</span>|] `shouldBe` Just "/yo/span"

    it "parses u- properties" $ do
      let dt = extractProperty Dt "updated" . documentRoot . parseLBS
      dt [r|<time class="dt-updated" datetime="ti.me">someday</time>|] `shouldBe` Just "ti.me"
      dt [r|<ins class="dt-updated" datetime="i.ns">someday</ins>|] `shouldBe` Just "i.ns"
      dt [r|<del class="dt-updated" datetime="d.el">someday</del>|] `shouldBe` Just "d.el"
      dt [r|<abbr class="dt-updated" title="ab.br">AB</abbr>|] `shouldBe` Just "ab.br"
      dt [r|<data class="dt-updated" value="da.ta"/>|] `shouldBe` Just "da.ta"
      dt [r|<input class="dt-updated" value="i.np.ut"/>|] `shouldBe` Just "i.np.ut"
      dt [r|<span class="dt-updated">
            <abbr class="value" title="vcp">VCP</abbr>
            <time class="value" datetime="ti">TIME</time>
            <ins class="value" datetime="me">lol</time>
          </span>|] `shouldBe` Just "vcptime"
      dt [r|<span class="dt-updated">date</span>|] `shouldBe` Just "date"

    it "parses e- properties" $ do
      let ct = extractProperty E "content" . documentRoot . parseLBS
      ct [r|<div class="e-content"><em>hello html</em>!</div>|] `shouldBe` Just "<em>hello html</em>!"

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
