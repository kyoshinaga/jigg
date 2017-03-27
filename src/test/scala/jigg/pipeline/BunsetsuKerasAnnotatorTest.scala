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

import scala.xml.{Elem, NodeSeq, XML, Node}

class BunsetsuKerasAnnotatorTest extends FunSuite {

  def findPath(localPath: String): String = getClass.getClassLoader.getResource(localPath).getPath

  /*
  def segment(node: Node, properties: Properties): NodeSeq = {
    val bunsetsuSplitter = new BunsetsuKerasAnnotator("bunsetsuKeras", properties)
    bunsetsuSplitter.newSentenceAnnotation(node)
//    pipeline.annotateText(text) \\ "sentences" \ "sentence" \\ "tokens" \ "token"
  }

  val properties = new Properties
  properties.setProperty("bunsetsuKeras.model", findPath("./data/keras/bunsetsu_model_sample.h5"))
  properties.setProperty("bunsetsuKeras.table", findPath("data/keras/jpnLookupWords.json"))
  */

  test("split text to bunsetsu") {
    import jigg.ml.keras.KerasParser

    val parser = KerasParser(
      findPath("./data/keras/bunsetsu_model_sample.h5"),
      findPath("./data/keras/jpnLookupWords.json")
    )

    val offsets = parser.parsing(Sentences.xml("oneSentence"))

    println(offsets)

    1 should be (1)
  }

  object Sentences {
    val xml = Map("oneSentence" ->
      <root>
       <document id="d1">
          <sentences>
            <sentence id="s1" characterOffsetBegin="0" characterOffsetEnd="6">
              梅が咲いた。
              <tokens annotators="mecab">
                <token id="s1_tok0" form="梅" offsetBegin="0" offsetEnd="1" pos="名詞" pos1="一般" pos2="*" pos3="*" cType="*" cForm="*" lemma="梅" yomi="ウメ" pron="ウメ"/>
                <token id="s1_tok1" form="が" offsetBegin="1" offsetEnd="2" pos="助詞" pos1="格助詞" pos2="一般" pos3="*" cType="*" cForm="*" lemma="が" yomi="ガ" pron="ガ"/>
                <token id="s1_tok2" form="咲い" offsetBegin="2" offsetEnd="4" pos="動詞" pos1="自立" pos2="*" pos3="*" cType="五段・カ行イ音便" cForm="連用タ接続" lemma="咲く" yomi="サイ" pron="サイ"/>
                <token id="s1_tok3" form="た" offsetBegin="4" offsetEnd="5" pos="助動詞" pos1="*" pos2="*" pos3="*" cType="特殊・タ" cForm="基本形" lemma="た" yomi="タ" pron="タ"/>
                <token id="s1_tok4" form="。" offsetBegin="5" offsetEnd="6" pos="記号" pos1="句点" pos2="*" pos3="*" cType="*" cForm="*" lemma="。" yomi="。" pron="。"/>
              </tokens>
            </sentence>
          </sentences>
        </document>
      </root>
    )
  }
}
