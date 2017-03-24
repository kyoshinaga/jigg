package jigg.pipeline

/*
 Copyright 2013-2015 Hiroshi Noji
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
     http://www.apache.org/licencses/LICENSE-2.0
     
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitation under the License.
*/

import java.util.Properties

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.xml.{Elem, NodeSeq, XML}

class BunsetsuKerasAnnotatorTest extends FunSuite {

  def segment(text: String, properties: Properties): NodeSeq = {
    val pipeline = new Pipeline(properties)
    pipeline.annotateText(text) \\ "sentences" \ "sentence" \\ "tokens" \ "token"
  }

  def findPath(localPath: String): String = getClass.getClassLoader.getResource(localPath).getPath

  val properties = new Properties
  properties.setProperty("annotators", "ssplitKeras,bunsetsuKeras")
  properties.setProperty("ssplitKeras.model", findPath("./data/keras/ssplit_model.h5"))
  properties.setProperty("ssplitKeras.table", findPath("./data/keras/jpnLookup.json"))
  properties.setProperty("bunsetsuKeras.model", findPath("./data/keras/bunsetsu_model.h5"))
  properties.setProperty("bunsetsuKeras.table", findPath("./data/keras/jpnLookup.json"))

  test("split text to bunsetsu") {
    val tokens = segment("梅が咲いた。", properties)

    tokens.length should be (2)
    (tokens(0) \\ "@form").text should be ("梅が")
    (tokens(1) \\ "@form").text should be ("咲いた。")
  }

  test("split text containing `Hankaku` and `Zenkaku` character"){
    val text =
      "半角文字abcdを含んでいる。" +
      "半角文字1234を含んでいる。" +
      "全角文字１２３４を含んでいる。"
    val tokens = segment(text, properties)

    tokens.length should be (6)
    (tokens(0) \\ "@form").text should be ("半角文字abcdを")
    (tokens(1) \\ "@form").text should be ("含んでいる。")
    (tokens(2) \\ "@form").text should be ("半角文字1234を")
    (tokens(3) \\ "@form").text should be ("含んでいる。")
    (tokens(4) \\ "@form").text should be ("全角文字１２３４を")
    (tokens(5) \\ "@form").text should be ("含んでいる。")
  }

  test("split text containing unknown character"){
    val text =
      "αを含んでいる。" +
      "βを含んでいる。"
    val tokens = segment(text, properties)

    tokens.length should be (4)
    (tokens(0) \\ "@form").text should be ("αを")
    (tokens(1) \\ "@form").text should be ("含んでいる。")
    (tokens(2) \\ "@form").text should be ("βを")
    (tokens(3) \\ "@form").text should be ("含んでいる。")
  }
}
