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

class SsplitKerasAnnotatorTest extends FunSuite {

  def rootNode(text: String) = <root><document>{ text }</document></root>

  def segment(text: String, properties: Properties) = {
    val ssplit = new SsplitKerasAnnotator("ssplitKeras", properties)
    val root = rootNode(text)
    ssplit.annotate(root) \ "document" \ "sentences" \ "sentence"
  }

  def findPath(localPath: String) = getClass.getClassLoader.getResource(localPath).getPath

  test("split sentence by point") {
    val properties = new Properties
    properties.setProperty("ssplitKeras.model", findPath("./data/keras/ssplit_model.h5"))
    properties.setProperty("ssplitKeras.table", findPath("./data/keras/jpnLookup.json"))
    val sentences = segment("梅が咲いた。\n桜も咲いた。", properties)

    println(sentences.mkString(","))

    sentences.length should be (2)
    sentences(0).text should be ("梅が咲いた。")
    sentences(1).text should be ("桜も咲いた。")
  }

}
